{-# LANGUAGE TypeOperators #-}

module Matrix where

import System.Random.MWC

import Data.Array.Accelerate
import Data.Array.Accelerate.Examples.Internal

type Matrix a = Array DIM2 a

-- A randomly generated matrix of given size
--
{-# INLINE randomMatrix #-}
randomMatrix
    :: (Variate a, Num a, Elt a)
    => DIM2 :~> a
    -> Int
    -> Int
    -> IO (Matrix a)
randomMatrix f rows cols = randomArrayIO f (Z:.rows:.cols)

-- A randomly generated vector of a given size
--
{-# INLINE randomVector #-}
randomVector
    :: (Variate a, Num a, Elt a)
    => DIM1 :~> a
    -> Int
    -> IO (Vector a)
randomVector f n = randomArrayIO f (Z:.n)
