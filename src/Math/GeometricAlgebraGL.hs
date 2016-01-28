{-# LANGUAGE ParallelListComp #-}

module Math.GeometricAlgebraGL
( GA
, mkSig
, GASig
, gaBasis
, gaVector
, gaNorm
, gaZero
, gaAdd
, gaReversion
, gaMult
, gaScalar
, gaScalarMult
, gaPart
, gaGrades
, gaOuterProd
, gaLeftContraction
, gaPseudoscalar
, gaPseudoinverse
, gaDual
, gaScalarPart
) where

import Graphics.Rendering.OpenGL.GL

import Data.Bits
import Data.Foldable
import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

newtype GASig =
   GASig
   { sigElements :: ([GLdouble])
   }

instance Show GASig where
   show = show . sigElements
instance Ord GASig where
   compare x y = compare (sigElements x) (sigElements y)
instance Eq GASig where
   x == y = sigElements x == sigElements y

data GA
  = GAScalar GLdouble
  | GA { gaSig :: {-# UNPACK #-} !GASig
       , gaMap :: IntMap GLdouble
       }
 deriving ( Show )


instance Eq GA where
  GAScalar x == GAScalar y       = x == y
  GAScalar x == GA{ gaMap = m } = m == IntMap.singleton 0 x
  GA{ gaMap = m } == GAScalar y = m == IntMap.singleton 0 y
  x@GA{} == y@GA{} = gaSig x == gaSig y && gaMap x == gaMap y

instance Ord GA where
  compare (GAScalar x) (GAScalar y)      = compare x y
  compare (GAScalar x) y@GA{ gaMap = m } = compare (IntMap.singleton 0 x) m
  compare x@GA{ gaMap = m } (GAScalar y) = compare m (IntMap.singleton 0 y)
  compare x@GA{} y@GA{} =
    case compare (gaSig x) (gaSig y) of
       LT -> LT
       GT -> GT
       EQ -> compare (gaMap x) (gaMap y)

instance Num GA where
  x + y = gaAdd x y
  x * y = gaMult x y
  negate x = gaScalarMult (-1) x
  fromInteger = GAScalar . fromInteger
  abs _ = error "Geometric Algebra does not support absolute value"
  signum _ = error "Geometric Algebra does not support signum"

instance Fractional GA where
  fromRational = GAScalar . fromRational
  recip (GAScalar x) = GAScalar (recip x)
  recip x
    | gaGrades x == IntSet.singleton 0 =
        case IntMap.lookup 0 (gaMap x) of
          Just a | a /= 0 -> GAScalar $ recip a
          Nothing -> error "Geometric Algebra: cannot recip the 0 scalar"
    | otherwise  = error "Geometric Algebra does not support recip except for scalars"


okSig :: GASig -> b -> b
okSig sig x
  | length (sigElements sig) <= finiteBitSize (0::Int) = x
  | otherwise = error $ unwords ["GA signature too large"]

sameSig :: GA -> GA -> b -> b
sameSig m n x
  | gaSig m == gaSig n = x
  | otherwise = error $ unwords ["incompatible signatures:"
                                , show (gaSig m), show (gaSig n)
                                ]

gaScalarPart :: GA -> GLdouble
gaScalarPart (GAScalar x) = x
gaScalarPart GA{ gaMap = m } = fromMaybe 0 (IntMap.lookup 0 m)

gaPart :: Int -> GA -> GA
gaPart 0 x@(GAScalar _) = x
gaPart 0 x              = GAScalar $ fromMaybe 0 $ IntMap.lookup 0 (gaMap x)
gaPart k (GAScalar _)   = gaZero
gaPart k x =
  x{ gaMap = IntMap.filterWithKey (\i _ -> popCount i == k) (gaMap x) }

gaGrades :: GA -> IntSet
gaGrades (GAScalar c)
  | c /= 0    = IntSet.singleton 1
  | otherwise = IntSet.empty
gaGrades x@GA{} = IntSet.fromList
  [ popCount i
  | (i,c) <- IntMap.toList (gaMap x)
  , c /= 0
  ]

gaZero :: GA
gaZero = GAScalar 0

gaNorm :: GA -> GLdouble
gaNorm (GAScalar x) = x*x
gaNorm GA{ gaMap = m }= sum $ fmap (\x -> x*x) $ m

gaScalar :: GLdouble -> GA
gaScalar = GAScalar

gaScalarMult :: GLdouble -> GA -> GA
gaScalarMult x (GAScalar y) = GAScalar (x*y)
gaScalarMult x mv@GA{ gaMap = m } = mv{ gaMap = fmap (x*) m }


maybeAdd :: GLdouble -> Maybe GLdouble -> Maybe GLdouble
maybeAdd x Nothing  = Just x
maybeAdd x (Just y) = Just (x+y)

gaAdd :: GA -> GA -> GA
gaAdd (GAScalar x) (GAScalar y)      = GAScalar (x+y)
gaAdd (GAScalar x) y@GA{ gaMap = m } = y{ gaMap = IntMap.alter (maybeAdd x) 0 m }
gaAdd x@GA{ gaMap = m } (GAScalar y) = x{ gaMap = IntMap.alter (maybeAdd y) 0 m }
gaAdd x@GA{} y@GA{} = sameSig x y $
    x{ gaMap = IntMap.unionWith (+) (gaMap x) (gaMap y) }

basisVec :: Int -> IntMap GLdouble
basisVec i = IntMap.singleton (shiftL (1::Int) i) 1

gaBasis :: GASig -> [GA]
gaBasis sig = okSig sig $
  [ GA sig (basisVec i) | i <- [0 .. length (sigElements sig)-1] ]

gaVector :: GASig -> [GLdouble] -> GA
gaVector sig xs
  | length (sigElements sig) == length xs = okSig sig $ GA sig $
     IntMap.fromList
     [ (shiftL (1::Int) i, x)
     | i <- [0 .. length (sigElements sig)-1]
     | x <- xs
     ]
  | otherwise = error $ unwords
                [ "gaVecotr: incorrect vector length"
                , show (sigElements sig), show xs
                ]


gaPseudoscalar :: GASig -> GA
gaPseudoscalar sig = okSig sig $ GA sig $ IntMap.singleton k 1
   where k = 2^n - 1
         n = length (sigElements sig)

gaPseudoinverse :: GASig -> GA
gaPseudoinverse sig = okSig sig $ GA sig $ IntMap.singleton k x
   where k = 2^n - 1
         n = length (sigElements sig)
         x = swapSign ((n * (n-1)) `div` 2) 1

gaReversion :: GA -> GA
gaReversion (GAScalar x) = GAScalar x
gaReversion ga@GA{ gaMap =  m} = ga{ gaMap = IntMap.mapWithKey f m }
  where f k x = swapSign ((n * (n-1)) `div` 2) $ x
          where n = popCount k

gaParityConjugation :: GA -> GA
gaParityConjugation (GAScalar x) = GAScalar x
gaParityConjugation ga@GA{ gaMap = m } = ga{ gaMap = IntMap.mapWithKey f m }
  where f k x = swapSign n $ x
          where n = popCount k

gaDual :: GA -> GA
gaDual x = gaMult x (gaPseudoinverse (gaSig x))

gaLeftContraction :: GA -> GA -> GA
gaLeftContraction x y = sameSig x y $
  foldr gaAdd gaZero
  [ gaPart (j-i) $ gaMult (gaPart i x) (gaPart j y)
  | i <- [0 .. n]
  , j <- [0 .. n]
  , i <= j
  ]
 where n = length (sigElements (gaSig x))

gaOuterProd :: GA -> GA -> GA
gaOuterProd x y = sameSig x y $
  foldr gaAdd gaZero
  [ gaPart (i+j) $ gaMult (gaPart i x) (gaPart j y)
  | i <- [0 .. n]
  , j <- [0 .. n]
  , i+j <= n
  ]
 where n = length (sigElements (gaSig x))

gaMult :: GA -> GA -> GA
gaMult (GAScalar x) (GAScalar y) = GAScalar (x*y)
gaMult (GAScalar x) y@GA{} = gaScalarMult x y
gaMult x@GA{} (GAScalar y) = gaScalarMult y x
gaMult x y = sameSig x y $
   x{ gaMap = IntMap.unionsWith (+) xs }

 where xs = [ gaMultOne (gaSig x) i j a b
            | (i,a) <- IntMap.toList (gaMap x)
            , (j,b) <- IntMap.toList (gaMap y)
            ]

gaMultOne :: GASig -> Int -> Int -> GLdouble -> GLdouble -> IntMap GLdouble
gaMultOne sig i j a b = IntMap.singleton k c
  where k = xor i j
        c = convConstant (sigElements sig) i j * a * b
--        c = conversionMemoMap sig i j * a * b

mkSig :: [GLdouble] -> GASig
mkSig sigElts = GASig sigElts


convConstant :: [GLdouble] -> Int -> Int -> GLdouble
convConstant = go 0 1
 where go _    ans []     _ _ = ans
       go bit  ans (x:xs) i j
          | testBit i bit && testBit j bit = go (bit+1)
                                                (x * swapSign (popCount i') ans)
                                                xs i' j'

          | testBit j bit                  = go (bit+1)
                                                (swapSign (popCount i') ans)
                                                xs i' j'

          | otherwise                      = go (bit+1)
                                                ans
                                                xs i' j'

        where i' = clearBit i bit
              j' = clearBit j bit

{-# INLINE swapSign #-}
swapSign :: Int -> GLdouble -> GLdouble
swapSign i x
  | even i    = x
  | otherwise = negate x
