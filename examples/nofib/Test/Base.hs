{-# LANGUAGE TypeOperators #-}

module Test.Base where

import Prelude                                          as P
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Array.Sugar


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

