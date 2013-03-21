
module Solver.Naive
  where

import Common.Type
import Common.Body
import Common.Util

import Data.Array.Accelerate                    as A


-- | Calculate accelerations on these particles in a naïve O(n^2) way
--
calcAccels :: Exp R -> Acc (Vector Body) -> Acc (Vector Accel)
calcAccels epsilon bodies
  = let n       = A.size bodies

        cols    = A.replicate (lift $ Z :. n :. All) bodies
        rows    = A.replicate (lift $ Z :. All :. n) bodies

    in
    A.fold (.+.) (vec 0) $ A.zipWith (accel epsilon) rows cols

