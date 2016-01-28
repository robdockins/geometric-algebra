{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}


import Control.Exception
import Control.Concurrent.Chan

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU.Matrix as GLU

import Data.IORef
import qualified Data.Foldable as Fold
import Control.Exception
import Control.Monad
import Control.Concurrent.MVar
import Data.StateVar
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import System.IO.Unsafe

import Math.GeometricAlgebraGL
import Control.FRPNow.Core
import Control.FRPNow.EvStream
import Control.FRPNow.GLFW

import qualified Debug.Trace as Debug

initialWinSize :: (Int,Int)
initialWinSize = (900,900)

main :: IO ()
main = runGLFW mkwin setup

mkwin :: IO (Maybe (GLFW.Window))
mkwin =
  GLFW.createWindow
     (fst initialWinSize)
     (snd initialWinSize)
     "Gravity pool"
     Nothing
     Nothing

evStreamTransducer
  :: forall a b st
   . (st -> b -> (st, Maybe a))
  -> st
  -> EvStream b
  -> Behavior (EvStream a)
evStreamTransducer update init evs = do
  let f :: (st, Maybe a) -> b -> (st, Maybe a)
      f (st, _) b = update st b
  filterMapEs snd <$> scanlEv f (init, Nothing) evs

setup :: GLFW.Window
      -> Behavior Double
      -> EvStream (Double, Double)
      -> Now (Behavior (IO ()))
setup win clock timer = do
   kevs   <- keyEvents win
   mbevs  <- mouseButtonEvents win
   rszevs <- resizeEvents win
   pos    <- mousePos win

   sync $ resize win initialWinSize
   callIOStream (resize win) rszevs

   let allEvs =
          fmap ResizeEv rszevs
          `merge`
          fmap KeyEv kevs
          `merge`
          fmap MouseEv mbevs
          `merge`
          fmap FrameEv timer

   let upd st ev = foldl updateState st (filterEv ev)
   states <- sampleNow $ foldEs upd initialState allEvs

   let renders = renderFrame win <$> states <*> pos

   sync $ GLFW.swapInterval 1
   sync $ GLFW.setCursorInputMode win GLFW.CursorInputMode'Hidden

   return renders

nearPlane :: GL.GLdouble
nearPlane =  0.5

farPlane :: GL.GLdouble
farPlane  = 1000.0


data State
 = State
   { pos :: V
   , epi :: V
   , polyR :: V
   , col :: C
   , rate :: GL.GLdouble
   , sides :: Int
   , pstart :: Maybe (GL.GLdouble, V)
   , particles :: Seq Particle
   , winSize :: (Int, Int)
   , displaySpinner :: Bool
   }

type Particle = (GL.GLdouble, V, V)

swap (GL.Color4 a b c d) = GL.Color4 c a b d

data Ev
 = FrameEv (Double,Double)
 | KeyEv KeyEv
 | MouseEv MouseEv
 | ResizeEv (Int,Int)

data GameEv
 = IncSides Int
 | IncSpin Int
 | ToggleSpinner
 | PlaceSpinner (Double, Double)
 | StartParticle (Double, Double)
 | EndParticle (Double, Double)
 | IncMass Int
 | SwapColor
 | TimerEvent Double
 | Resize (Int,Int)

filterEv :: Ev -> [GameEv]
filterEv (KeyEv (k,kst,mod))
  |   (kst == GLFW.KeyState'Pressed || kst == GLFW.KeyState'Repeating)
  &&  k   == GLFW.Key'Left
  = [IncSides (-1)]

  |   (kst == GLFW.KeyState'Pressed || kst == GLFW.KeyState'Repeating)
  &&  k   == GLFW.Key'Right
  = [IncSides 1]

  |   (kst == GLFW.KeyState'Pressed || kst == GLFW.KeyState'Repeating)
  &&  k   == GLFW.Key'Enter
  = [ToggleSpinner]

  |   (kst == GLFW.KeyState'Pressed || kst == GLFW.KeyState'Repeating)
  &&  k   == GLFW.Key'Down
  = [IncSpin (-1)]

  |   (kst == GLFW.KeyState'Pressed || kst == GLFW.KeyState'Repeating)
  &&  k   == GLFW.Key'Up
  = [IncSpin 1]

  |   (kst == GLFW.KeyState'Pressed || kst == GLFW.KeyState'Repeating)
  &&  k   == GLFW.Key'Equal
  = [IncMass 1]

  |   (kst == GLFW.KeyState'Pressed || kst == GLFW.KeyState'Repeating)
  &&  k   == GLFW.Key'Minus
  = [IncMass (-1)]

  | kst == GLFW.KeyState'Pressed
  = [SwapColor]

filterEv (MouseEv (btn,btnst,_,x,y))
  |    btn == GLFW.MouseButton'1
  && btnst == GLFW.MouseButtonState'Pressed
  = [PlaceSpinner (x,y)]

  |    btn == GLFW.MouseButton'2
  && btnst == GLFW.MouseButtonState'Pressed
  = [StartParticle (x,y)]

  |    btn == GLFW.MouseButton'2
  && btnst == GLFW.MouseButtonState'Released
  = [EndParticle (x,y)]

filterEv (FrameEv (_,dt))
  = [TimerEvent dt]

filterEv (ResizeEv (h,w))
  = [Resize (h,w)]

filterEv _ = []


updateState :: State
            -> GameEv
            -> State
updateState st (IncSides x) =
  st{ sides = sides st + x }

updateState st (IncSpin x) =
  st{ rate = rate st + (realToFrac x) * 0.1 }

updateState st (IncMass x)
  | Just (mass, start) <- pstart st =
  st{ pstart = Just (mass * 1.1**(realToFrac x), start) }

updateState st ToggleSpinner =
  st{ displaySpinner = not $ displaySpinner st }

updateState st SwapColor =
  st{ col = swap (col st) }

updateState st (PlaceSpinner (x,y)) =
  st{ pos = invertMouse (winSize st) (realToFrac x, realToFrac y) }

updateState st (StartParticle (x,y)) =
  st{ pstart = Just (2e6, invertMouse (winSize st) (realToFrac x, realToFrac y)) }

updateState st (EndParticle (x,y))
  | Just (mass,start) <- pstart st =
    let end = invertMouse (winSize st) (realToFrac x,realToFrac y)
        diff = end - start - gaScalarMult 0.01 e2
        newp = (mass, start, diff)
     in st{ particles = newp Seq.<| particles st
          , pstart    = Nothing
          }

updateState st (Resize sz) =
  st{ winSize = sz }

updateState st (TimerEvent dt') =
  let dt = realToFrac dt' in
  st{ pos = updatePos dt (pos st)
    , epi = updateEpi (rate st * dt) (epi st)
    , polyR = updateEpi (-3.0 * rate st * dt) (polyR st)
    , particles = updateParticles dt (particles st)
    }


g :: GL.GLdouble
g = 6.673e-11

updateParticles :: GL.GLdouble -> Seq Particle -> Seq Particle
updateParticles dt ps =
  let len = Seq.length ps
      forces = Seq.fromList
               [ sum [ (realToFrac (m1*m2*g))*diff / (realToFrac (dist*norm))
                     | j <- [0..len-1]
                     , i /= j
                     , let (m2,p2,_) = Seq.index ps j
                     , let diff = p2-p1
                     , let norm = gaNorm diff
                     , let dist = sqrt norm
                     ]
               | i <- [0..len-1]
               , let (m1,p1,_) = Seq.index ps i
               ]
      upd (mass,pos,vel) f =
         let vel' = f*(realToFrac (dt/mass)) + vel
             pos' = vel'*(realToFrac dt) + pos
          in (mass, pos', vel')

      gravUpdate = Seq.zipWith upd ps forces

    in collideParticles 0 dt ps gravUpdate

filterCollide :: [(a,b,Maybe c)] -> [(a,b,c)]
filterCollide [] = []
filterCollide ((_,_,Nothing):xs) = filterCollide xs
filterCollide ((i,j,Just x) :xs) = (i,j,x) : filterCollide xs

firstCollision :: [(Int,Int,(GL.GLdouble,[Particle],[Particle]))] ->
                   Maybe (Int,Int,(GL.GLdouble,[Particle],[Particle]))
firstCollision = foldl f Nothing
 where
  f Nothing x = Just x
  f (Just x@(_,_,(t1,_,_)))
    y@(_,_,(t2,_,_)) = if t1 < t2 then Just x else Just y

deleteFromSeq :: Int -> Seq a -> Seq a
deleteFromSeq i seq =
  assert (i < Seq.length seq) $
     front Seq.>< (Seq.drop 1 back)
 where (front, back) = Seq.splitAt i seq

advanceParticle :: GL.GLdouble -> Particle -> Particle
advanceParticle dt (mass,pos,vel) = (mass, pos + gaScalarMult dt vel, vel)

collideParticles :: GL.GLdouble -> GL.GLdouble -> Seq Particle -> Seq Particle -> Seq Particle
collideParticles min_soln dt start end =
   case firstCollision allCollisions of
     Nothing -> end
     Just (i,j,(t,newstart,newend)) ->
       let start'  = fmap (advanceParticle (t*dt)) $ deleteFromSeq i $ deleteFromSeq j start
           end'    = deleteFromSeq i $ deleteFromSeq j end
           start'' = Seq.fromList newstart Seq.>< start'
           end''   = Seq.fromList newend   Seq.>< end'
        in collideParticles 0.005 ((1-t)*dt) start'' end''

  where n = Seq.length start

        allCollisions = filterCollide
                        [ (i,j,tryCollide1 min_soln dt
                              (Seq.index start i, Seq.index end i)
                              (Seq.index start j, Seq.index end j))
                        | i <- [0   .. n-1]
                        , j <- [i+1 .. n-1]
                        ]


pmass :: Particle -> GL.GLdouble
pmass (m,_,_) = m

ppos :: Particle -> V
ppos (_,p,_) = p

pvel :: Particle -> V
pvel (_,_,v) = v

tryCollide1 :: GL.GLdouble
            -> GL.GLdouble
            -> (Particle, Particle)
            -> (Particle, Particle)
            -> Maybe (GL.GLdouble, [Particle], [Particle])
tryCollide1 min_soln dt (start1, end1) (start2, end2) =
  if discrim >= 0 && min_soln < soln && soln <= 1
     then {-Debug.trace (unlines [ "dt = " ++ show dt
                               , "COLLISION TIME = " ++ show soln
                               , "COLLISION: pos = " ++ show collision_pos
                               , "MOMENTUM CHANGE = " ++ show momentum_diff
                               , "MOMENTUM RATIO = " ++ show
                                    ((sqrt (gaNorm momentum_start))
                                    /
                                    (sqrt (gaNorm momentum_end)))
                               , "KE RATIO: = " ++ show ke_ratio
                               , "u1 = " ++ show u1
                               , "u2 = " ++ show u2
                               , "v1 = " ++ show v1
                               , "v2 = " ++ show v2
                               , "r1 = " ++ show r1
                               , "r2 = " ++ show r2
                               , "f(soln) = " ++ show f_soln
                               , "soln_pos1 = " ++ show soln_pos1
                               , "soln_pos2 = " ++ show soln_pos2
                               , "|soln_pos2 - soln_pos1| = " ++ show (sqrt (gaNorm (soln_pos2 - soln_pos1)))
                               , "|normal| = " ++ show (sqrt (gaNorm normal))
                               , "|vel1 - (proj1 + rej1)| = " ++ show
                                      (sqrt (gaNorm (pvel start1 - (proj_vel1 + rej_vel1))))
                               , "|vel2 - (proj2 + rej2)| = " ++ show
                                      (sqrt (gaNorm (pvel start2 - (proj_vel2 + rej_vel2))))

                               , "POS1 = " ++ show final_pos1
                               , "VEL1 = " ++ show final_vel1
                               , "POS2 = " ++ show final_pos2
                               , "VEL2 = " ++ show final_vel2

                               , "velocity_diff = " ++ show velocity_diff
                               ]
                 ) $
-}
                 if velocity_diff > fuse_threshold
                    then Just ( soln
                              , [(pmass start1, soln_pos1, final_vel1)
                                ,(pmass start2, soln_pos2, final_vel2)
                                ]
                              , [(pmass start1, final_pos1, final_vel1)
                                ,(pmass start2, final_pos2, final_vel2)
                                ]
                              )
                    else --Debug.trace "FUSED!" $
                          Just (soln
                               , [ (fused_mass, fused_pos_start, fused_vel) ]
                               , [ (fused_mass, fused_pos_end, fused_vel) ]
                               )

     else Nothing

 where r1 = particleRadius start1
       r2 = particleRadius start2

       momentum_start = gaScalarMult m1 (pvel start1)
                      + gaScalarMult m2 (pvel start2)
       momentum_end   = gaScalarMult m1 final_vel1
                      + gaScalarMult m2 final_vel2
       momentum_diff = sqrt (gaNorm (momentum_end - momentum_start))

       ke_start = m1 * gaNorm (pvel start1)
                + m2 * gaNorm (pvel start2)
       ke_end   = m1 * gaNorm final_vel1
                + m2 * gaNorm final_vel2

       ke_ratio = ke_end / ke_start

       d_start = ppos start2 - ppos start1
       d_end   = ppos end2 - ppos end1
       d_diff  = d_end - d_start

       a = gaNorm d_diff
       b = 2 * (gaScalarPart $ gaLeftContraction d_start d_diff)
       c = gaNorm d_start - (r1+r2)^2

       discrim = b^2 - 4*a*c
       soln = (-b - sqrt discrim) / (2*a)

       f_soln = a*soln*soln + b*soln + c

       soln_pos1 = ppos start1 + gaScalar soln * (ppos end1 - ppos start1)
       soln_pos2 = ppos start2 + gaScalar soln * (ppos end2 - ppos start2)
       collision_pos  = soln_pos1 + gaScalar (r1 / (r1+r2)) * (soln_pos2 - soln_pos1)

       normal = gaScalarMult (1/r1) (soln_pos1 - collision_pos)
       normal_inv = gaReversion normal

       proj_vel1 = (gaLeftContraction (pvel start1) normal) * normal_inv
       rej_vel1  = (gaOuterProd (pvel start1) normal) * normal_inv

       proj_vel2 = (gaLeftContraction (pvel start2) normal) * normal_inv
       rej_vel2  = (gaOuterProd (pvel start2) normal) * normal_inv

       u1 = -sqrt (gaNorm proj_vel1)
       u2 =  sqrt (gaNorm proj_vel2)

       m1 = pmass start1
       m2 = pmass start2

       v1 = (m1*u1 + m2*u2 + m2*coefficent_of_restitution*(u2 - u1)) / (m1+m2)
       v2 = (m1*u1 + m2*u2 + m1*coefficent_of_restitution*(u1 - u2)) / (m1+m2)

       final_vel1 = rej_vel1 + gaScalarMult v1 normal
       final_vel2 = rej_vel2 + gaScalarMult v2 normal

       final_pos1 = soln_pos1 + gaScalarMult (dt * (1.0 - soln)) final_vel1
       final_pos2 = soln_pos2 + gaScalarMult (dt * (1.0 - soln)) final_vel2

       coefficent_of_restitution = 0.3

       velocity_diff = sqrt (gaNorm (final_vel2 - final_vel1))

       fuse_threshold = 0.1

       fused_mass = m1 + m2
       fused_vel  = gaScalarMult (m1/fused_mass) (pvel start1)
                  + gaScalarMult (m2/fused_mass) (pvel start2)
       fused_pos_start
                  = soln_pos1 + gaScalarMult (m2/fused_mass) (soln_pos2 - soln_pos1)
       fused_pos_end
                  = fused_pos_start
                  + gaScalarMult (dt * (1.0 - soln)) fused_vel

invertMouse :: (Int,Int) -> (Double,Double) -> V
{-
invertMouse (winWidth,winHeight) (x,y) =
  let GL.Vertex3 a b c = unsafePerformIO $ do
        (mv :: GL.GLmatrix GL.GLdouble) <- get (GL.matrix (Just (GL.Modelview 0)))
        (pj :: GL.GLmatrix GL.GLdouble) <- get (GL.matrix (Just (GL.Projection)))
        pos <- GLU.unProject
           (GL.Vertex3 (realToFrac x) (realToFrac y) 0.01)
           pj mv
           (GL.Position 0 0, GL.Size (fromIntegral winWidth) (fromIntegral winHeight))
        print pos
        return pos
   in gaVector sig3D (map realToFrac [a,b,-3])
-}
invertMouse (winWidth,winHeight) (x,y) =
  let h = realToFrac winHeight
      w = realToFrac winWidth
      (iw,ih) = initialWinSize
      x' =  (2*x - w) / realToFrac iw
      y' = -(2*y - h) / realToFrac ih
   in gaVector sig3D [realToFrac x', realToFrac y', 0]


initialState :: State
initialState = State
  (gaVector sig3D [ 0.9, 0, 0 ])
  (gaVector sig3D [ 0.15, 0, 0 ])
  (gaVector sig3D [ 0.1, 0, 0 ])
  (GL.Color4 0 1.0 0.5 1.0)
  1.0
  5
  Nothing
  Seq.empty
  initialWinSize
  True

resize :: GLFW.Window -> (Int,Int) -> IO ()
resize win (w,h) = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
--  GL.frustum l r b t (realToFrac nearPlane) (realToFrac farPlane)
  GL.ortho l r b t (realToFrac nearPlane) (realToFrac farPlane)
 where
  (iw,ih) = initialWinSize

  l = -r
  r = realToFrac w / realToFrac iw

  b = -t
  t = realToFrac h / realToFrac ih

renderFrame :: GLFW.Window -> State -> (Double, Double) -> IO ()
renderFrame win st mpos = do
  GL.clear [GL.ColorBuffer]

  mapM_ renderParticle (particles st)

  when (displaySpinner st) $ do
    GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
    regularPolygon 8 (0.3*(polyR st)) (pos st)

    GL.currentColor $= (col st)
    regularPolygon (sides st) (polyR st) (pos st + epi st)

  case pstart st of
    Just (mass,pos) -> do
       renderParticle (mass,pos,0)

    Nothing -> do
       let x = invertMouse (winSize st) mpos
       GL.currentColor $= GL.Color4 0.5 0.5 0.5 1.0
       regularPolygon 6 (gaVector sig3D [0.01,0,0]) x

particleRadius :: Particle -> GL.GLdouble
particleRadius (mass,_,_) =
  5.0e-5 * ((realToFrac mass) ** (1/3))

renderParticle :: Particle -> IO ()
renderParticle p@(mass,pos,_) = do
  let sz = particleRadius p
  GL.currentColor $= GL.Color4 0.9 0.1 0.1 1.0
  regularPolygon 30 (gaVector sig3D [sz,0,0]) pos


vertex :: V -> IO ()
vertex v = do
   let x = gaScalarPart $ gaMult e0 v
   let y = gaScalarPart $ gaMult e1 v
   let z = gaScalarPart $ gaMult e2 v
   let (x' :: GL.GLdouble) = realToFrac x
   let (y' :: GL.GLdouble) = realToFrac y
   let (z' :: GL.GLdouble) = realToFrac z
   GL.vertex (GL.Vertex3 x' y' (z'-1))

regularPolygon :: Int -> V -> V -> IO ()
regularPolygon n arm pos = do
  GL.renderPrimitive GL.Polygon $ sequence_
     [ vertex (pos + rot (2*pi*(fromIntegral i)/fromIntegral n) arm)
     | i <- [ 0 .. n-1 ]
     ]

type V = GA
type C = GL.Color4 GL.GLfloat

sig3D :: GASig
sig3D = mkSig [1.0, 1.0, 1.0]

basis3D :: [V]
basis3D = gaBasis sig3D

e0 = basis3D !! 0
e1 = basis3D !! 1
e2 = basis3D !! 2


i :: V
i = e0 * e1 * e2

-- NB only works for reflections by 1-blades (aka vectors)
reflectVec :: V -> V -> V
reflectVec b x = b * x * gaScalarMult (1 / gaNorm b) (gaReversion b)

rotOp :: GL.GLdouble -> V -> V
rotOp θ i =
  let v = gaVector sig3D [ cos (θ/2), sin (θ/2), 0 ]
   in ( e0 * v )

rot :: GL.GLdouble -> V -> V
rot θ v =
  let s = rotOp θ i
   in s * v * gaReversion s

updatePos :: GL.GLdouble -> V -> V
updatePos dt v = rot ((pi/4)*dt) v

updateEpi :: GL.GLdouble -> V -> V
updateEpi dt v = rot (3*pi*dt) v
