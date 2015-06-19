{-# LANGUAGE ParallelListComp #-}

module Math.GeometricAlgebra
( GA
, gaBasis
, gaVector
, gaNorm
, gaZero
, gaAdd
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
) where

import Data.Bits
import Data.Foldable
import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

type GASig a = [a]

data GA a
  = GAScalar a
  | GA { gaSig :: [a]
       , gaMap :: IntMap a
       }
 deriving ( Show )


instance (Eq a) => Eq (GA a) where
  GAScalar x == GAScalar y       = x == y
  GAScalar x == GA{ gaMap = m } = m == IntMap.singleton 0 x
  GA{ gaMap = m } == GAScalar y = m == IntMap.singleton 0 y
  x@GA{} == y@GA{} = gaSig x == gaSig y && gaMap x == gaMap y

instance (Ord a) => Ord (GA a) where
  compare (GAScalar x) (GAScalar y)      = compare x y
  compare (GAScalar x) y@GA{ gaMap = m } = compare (IntMap.singleton 0 x) m
  compare x@GA{ gaMap = m } (GAScalar y) = compare m (IntMap.singleton 0 y)
  compare x@GA{} y@GA{} =
    case compare (gaSig x) (gaSig y) of
       LT -> LT
       GT -> GT
       EQ -> compare (gaMap x) (gaMap y)

instance (Show a, Eq a, Num a) => Num (GA a) where
  x + y = gaAdd x y
  x * y = gaMult x y
  negate x = gaScalarMult (-1) x
  fromInteger = GAScalar . fromInteger
  abs _ = error "Geometric Algebra does not support absolute value"
  signum _ = error "Geometric Algebra does not support signum"

instance (Show a, Eq a, Fractional a) => Fractional (GA a) where
  fromRational = GAScalar . fromRational
  recip (GAScalar x) = GAScalar (recip x)
  recip x
    | gaGrades x == IntSet.singleton 0 =
        case IntMap.lookup 0 (gaMap x) of
          Just a | a /= 0 -> GAScalar $ recip a
          Nothing -> error "Geometric Algebra: cannot recip the 0 scalar"
    | otherwise  = error "Geometric Algebra does not support recip except for scalars"


okSig :: GASig a -> b -> b
okSig sig x
  | length sig <= finiteBitSize (0::Int) = x
  | otherwise = error $ unwords ["GA signature too large"]

sameSig :: (Show a, Eq a) => GA a -> GA a -> b -> b
sameSig m n x
  | gaSig m == gaSig n = x
  | otherwise = error $ unwords ["incompatible signatures:"
                                , show (gaSig m), show (gaSig n)
                                ]

gaPart :: Num a => Int -> GA a -> GA a
gaPart 0 x@(GAScalar _) = x
gaPart 0 x              = GAScalar $ fromMaybe 0 $ IntMap.lookup 0 (gaMap x)
gaPart k (GAScalar _)   = gaZero
gaPart k x =
  x{ gaMap = IntMap.filterWithKey (\i _ -> popCount i == k) (gaMap x) }

gaGrades :: (Eq a, Num a) => GA a -> IntSet
gaGrades (GAScalar c)
  | c /= 0    = IntSet.singleton 1
  | otherwise = IntSet.empty
gaGrades x@GA{} = IntSet.fromList
  [ popCount i
  | (i,c) <- IntMap.toList (gaMap x)
  , c /= 0
  ]

gaZero :: Num a => GA a
gaZero = GAScalar 0

gaNorm :: Num a => GA a -> a
gaNorm (GAScalar x) = x*x
gaNorm GA{ gaMap = m }= sum $ fmap (\x -> x*x) $ m

gaScalar :: a -> GA a
gaScalar = GAScalar

gaScalarMult :: Num a => a -> GA a -> GA a
gaScalarMult x (GAScalar y) = GAScalar (x*y)
gaScalarMult x mv@GA{ gaMap = m } = mv{ gaMap = fmap (x*) m }

maybeAdd :: Num a => a -> Maybe a -> Maybe a
maybeAdd x Nothing  = Just x
maybeAdd x (Just y) = Just (x+y)

gaAdd :: (Show a, Eq a, Num a) => GA a -> GA a -> GA a
gaAdd (GAScalar x) (GAScalar y)      = GAScalar (x+y)
gaAdd (GAScalar x) y@GA{ gaMap = m } = y{ gaMap = IntMap.alter (maybeAdd x) 0 m }
gaAdd x@GA{ gaMap = m } (GAScalar y) = x{ gaMap = IntMap.alter (maybeAdd y) 0 m }
gaAdd x@GA{} y@GA{} = sameSig x y $
    x{ gaMap = IntMap.unionWith (+) (gaMap x) (gaMap y) }

basisVec :: Num a => Int -> IntMap a
basisVec i = IntMap.singleton (shiftL (1::Int) i) 1

gaBasis :: Num a => GASig a -> [GA a]
gaBasis sig = okSig sig $
  [ GA sig (basisVec i) | i <- [0 .. length sig-1] ]

gaVector :: (Num a, Show a) => GASig a -> [a] -> GA a
gaVector sig xs
  | length sig == length xs = okSig sig $ GA sig $
     IntMap.fromList
     [ (shiftL (1::Int) i, x)
     | i <- [0 .. length sig-1]
     | x <- xs
     ]
  | otherwise = error $ unwords
                [ "gaVecotr: incorrect vector length"
                , show sig, show xs
                ]


gaPseudoscalar :: Num a => GASig a -> GA a
gaPseudoscalar sig = okSig sig $ GA sig $ IntMap.singleton k 1
   where k = 2^n - 1
         n = length sig

gaPseudoinverse :: Num a => GASig a -> GA a
gaPseudoinverse sig = okSig sig $ GA sig $ IntMap.singleton k x
   where k = 2^n - 1
         n = length sig
         x = swapSign $ (n * (n-1)) `div` 2

gaDual :: (Show a, Eq a, Num a) => GA a -> GA a
gaDual x = gaMult x (gaPseudoinverse (gaSig x))

gaLeftContraction :: (Show a, Eq a, Num a) => GA a -> GA a -> GA a
gaLeftContraction x y = sameSig x y $
  foldr gaAdd gaZero
  [ gaPart (j-i) $ gaMult (gaPart i x) (gaPart j y)
  | i <- [0 .. n]
  , j <- [0 .. n]
  , i <= j
  ]
 where n = length (gaSig x)

gaOuterProd :: (Show a, Eq a, Num a) => GA a -> GA a -> GA a
gaOuterProd x y = sameSig x y $
  foldr gaAdd gaZero
  [ gaPart (i+j) $ gaMult (gaPart i x) (gaPart j y)
  | i <- [0 .. n]
  , j <- [0 .. n]
  , i+j <= n
  ]
 where n = length (gaSig x)

gaMult :: (Show a, Eq a, Num a) => GA a -> GA a -> GA a
gaMult (GAScalar x) (GAScalar y) = GAScalar (x*y)
gaMult (GAScalar x) y@GA{} = gaScalarMult x y
gaMult x@GA{} (GAScalar y) = gaScalarMult y x
gaMult x y = sameSig x y $
   x{ gaMap = foldr (IntMap.unionWith (+)) IntMap.empty xs }

 where xs = [ gaMultOne (gaSig x) i j a b
            | (i,a) <- IntMap.toList (gaMap x)
            , (j,b) <- IntMap.toList (gaMap y)
            ]

gaMultOne :: Num a => GASig a -> Int -> Int -> a -> a -> IntMap a
gaMultOne sig i j a b = IntMap.singleton k c
  where k = xor i j
        c = convConstant sig i j * a * b

convConstant :: Num a => GASig a -> Int -> Int -> a
convConstant sig = go 0
 where go bit i j
          | bit > length sig = 1
          | testBit i bit && testBit j bit = (sig!!bit)
                                               * swapSign (popCount i')
                                               * (go (bit+1) i' j')
          | testBit j bit                  = swapSign (popCount i') * go (bit+1) i' j'
          | otherwise                      = go (bit+1) i' j'

        where i' = clearBit i bit
              j' = clearBit j bit

swapSign :: Num a => Int -> a
swapSign i
  | even i    =  1
  | otherwise = -1
