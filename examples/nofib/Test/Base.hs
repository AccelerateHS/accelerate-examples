{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Base where

import Prelude                                          as P
import Data.Complex
import Control.Monad                                    ( unless )
import Test.HUnit                                       ( Assertion, assertFailure )
import Test.QuickCheck
import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar                as Sugar


-- A class of things that support almost-equality, so that we can disregard
-- small amounts of floating-point round-off error.
--
class Similar a where
  {-# INLINE (~=) #-}
  (~=) :: a -> a -> Bool
  default (~=) :: Eq a => a -> a -> Bool
  (~=) = (==)

infix 4 ~=

instance Similar a => Similar [a] where
  []     ~= []          = True
  (x:xs) ~= (y:ys)      = x ~= y && xs ~= ys
  _      ~= _           = False

instance (Similar a, Similar b) => Similar (a, b) where
  (x1, x2) ~= (y1, y2) = x1 ~= y1 && x2 ~= y2

instance (Similar a, Similar b, Similar c) => Similar (a, b, c) where
  (x1, x2, x3) ~= (y1, y2, y3) = x1 ~= y1 && x2 ~= y2 && x3 ~= y3

instance (Similar a, Similar b, Similar c, Similar d) => Similar (a, b, c, d) where
  (x1, x2, x3, x4) ~= (y1, y2, y3, y4) = x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4

instance (Similar a, Similar b, Similar c, Similar d, Similar e)
    => Similar (a, b, c, d, e) where
  (x1, x2, x3, x4, x5) ~= (y1, y2, y3, y4, y5) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5

instance (Similar a, Similar b, Similar c, Similar d, Similar e, Similar f)
    => Similar (a, b, c, d, e, f) where
  (x1, x2, x3, x4, x5, x6) ~= (y1, y2, y3, y4, y5, y6) =
    x1 ~= y1 && x2 ~= y2 && x3 ~= y3 && x4 ~= y4 && x5 ~= y5 && x6 ~= y6


instance Similar Int
instance Similar Int8
instance Similar Int16
instance Similar Int32
instance Similar Int64
instance Similar Word
instance Similar Word8
instance Similar Word16
instance Similar Word32
instance Similar Word64
instance Similar Char
instance Similar Bool

instance Similar DIM1
instance Similar DIM2
instance Similar DIM3

instance Similar Float  where (~=) = absRelTol 0.00005 0.005
instance Similar Double where (~=) = absRelTol 0.00005 0.005

instance (Similar e, RealFloat e) => Similar (Complex e) where
  -- CUFFT can actually give quite large errors, so we have to
  -- increase the epsilon for the absolute relative value difference.
  (r1 :+ i1) ~= (r2 :+ i2) = r1 ~= r2 && i1 ~= i2


{-# INLINE relTol #-}
relTol :: (Fractional a, Ord a) => a -> a -> a -> Bool
relTol epsilon x y = abs ((x-y) / (x+y+epsilon)) < epsilon

{-# INLINE absRelTol #-}
absRelTol :: (RealFloat a, Ord a) => a -> a -> a -> a -> Bool
absRelTol epsilonAbs epsilonRel u v
  |  isInfinite u
  && isInfinite v          = True
  |  isNaN u
  && isNaN v               = True
  | abs (u-v) < epsilonAbs = True
  | abs u > abs v          = abs ((u-v) / u) < epsilonRel
  | otherwise              = abs ((v-u) / v) < epsilonRel

instance (Eq e, Eq sh, Shape sh) => Eq (Array sh e) where
  a1 == a2      =  arrayShape a1 == arrayShape a2
                && toList a1     == toList a2

  a1 /= a2      =  arrayShape a1 /= arrayShape a2
                || toList a1     /= toList a2

instance (Similar e, Eq sh, Shape sh) => Similar (Array sh e) where
  a1 ~= a2      =  arrayShape a1 == arrayShape a2
                && toList a1     ~= toList a2


-- | Assert that the specified actual value is equal-ish to the expected value.
-- If we are in verbose mode, the output message will contain the expected and
-- actual values.
--
assertEqual
    :: (Similar a, Show a)
    => a        -- ^ The expected value
    -> a        -- ^ The actual value
    -> Assertion
assertEqual expected actual =
  unless (expected ~= actual)
         (assertFailure (failure expected actual))

failure :: Show a => a -> a -> String
failure expected actual =
  unlines [ "*** Expected:", show expected
          , "*** Received:", show actual ]

infix 1 ~=?, ~?=

-- Short hand for a test case that asserts similarity, with the actual value on
-- the right hand side and the expected value on the left.
--
(~=?) :: (Similar a, Show a) => a -> a -> Property
expected ~=? actual = counterexample (failure expected actual) (expected ~= actual)

-- Short hand for a test case that asserts similarity, with the actual value on
-- the left hand side and the expected value on the right.
--
(~?=) :: (Similar a, Show a) => a -> a -> Property
(~?=) = flip (~=?)


-- Miscellaneous
--

indexHead :: Shape sh => (sh:.Int) -> Int
indexHead (_ :. sz) = sz

indexTail :: Shape sh => (sh:.Int) -> sh
indexTail (sh :. _) = sh

isEmptyArray :: Shape sh => Array sh e -> Bool
isEmptyArray arr = arraySize (arrayShape arr) == 0

mkDim :: Shape sh => Int -> sh
mkDim n = listToShape (P.replicate n 0)

dim0 :: DIM0
dim0 = mkDim 0

dim1 :: DIM1
dim1 = mkDim 1

dim2 :: DIM2
dim2 = mkDim 2

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = cycle [[]]
splitEvery n xs =
  let (h,t) = splitAt n xs
  in  h : splitEvery n t

splitPlaces :: Integral i => [i] -> [a] -> [[a]]
splitPlaces []     _  = []
splitPlaces (i:is) vs =
  let (h,t) = splitAt (P.fromIntegral i) vs
  in  h : splitPlaces is t

