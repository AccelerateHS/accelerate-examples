{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language ScopedTypeVariables #-}

module QuickSort (quicksort) where

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Unsafe as A
import qualified Data.Array.Accelerate.Data.Bits as A
import qualified Data.Array.Accelerate.Examples.Internal as A
import Control.Monad

quicksort :: A.Backend -> A.Vector Int -> A.Vector Int
quicksort backend = A.runN backend quicksort'

quicksort' :: A.Acc (A.Vector Int) -> A.Acc (A.Vector Int)
quicksort' input = result
  where
    emptyFlags = A.fill (A.index1 $ 1 + A.unindex1 (A.shape input)) (A.constant False)
    -- Initially, we have one segment, namely the whole array
    initialFlags = A.scatter (singleton 0 A.++ A.fill (A.index1 1) (A.unindex1 $ A.shape input)) emptyFlags (A.use $ A.fromList (A.Z A.:. 2) [True, True])

    -- We stop when each segment contains just one element, as segments of one element are sorted.
    A.T2 result _ = A.awhile condition step $ A.T2 input initialFlags

singleton :: A.Elt e => e -> A.Acc (A.Vector e)
singleton e = A.use $ A.fromList (A.Z A.:. 1) [e]

type State =
  -- Values
  ( A.Vector Int
  -- Head flags, denoting the starting points of the unsorted segments
  , A.Vector Bool
  )

step :: A.Acc State -> A.Acc State
step (A.T2 values headFlags) = (A.T2 values' headFlags')
  where
    -- Per element, the pivot of the segment of that element
    -- For each segment, we just take the first element as pivot
    pivots = propagateSegmentHead headFlags values

    -- Find which elements are larger than the pivot
    isLarger = A.zipWith (A.>=) values pivots

    -- Propagate the start index of a segment to all elements
    startIndex = propagateSegmentHead headFlags (A.generate (A.shape values) A.unindex1)

    -- Compute the offsets to which the elements must be moved using a scan
    indicesLarger, indicesSmaller :: A.Acc (A.Vector Int)
    indicesLarger  = A.map (\x -> x - 1) $ postscanSegHead (+) headFlags $ A.map (A.? (1, 0)) isLarger
    indicesSmaller = A.map (\x -> x - 1) $ postscanSegHead (+) headFlags $ A.map (A.? (0, 1)) isLarger

    -- Propagate the number of smaller elements to each segment
    -- This is needed as an offset for the larger elements
    countSmaller :: A.Acc (A.Vector Int)
    countSmaller = A.map (+1) $ propagateSegmentLast headFlags indicesSmaller

    -- Compute the new indices of the elements
    permutation = A.zipWith5 partitionPermuteIndex isLarger startIndex indicesSmaller indicesLarger countSmaller
    
    -- Perform the permutation
    values' = A.scatter permutation (A.fill (A.shape values) A.undef) values

    -- Update the head flags for the next iteration (the 'recursive call' in a traditional implementation)
    -- Mark new section starts at:
    -- * the position of the pivot
    -- * the position of the pivot + 1
    headFlags' =
      let
        f :: Int -> A.Exp Bool -> A.Exp Int -> A.Exp Int -> A.Exp A.DIM1
        f inc headF start countSmall =
          headF A.? (A.index1 $ start + countSmall + A.constant inc, A.ignore)
        writes :: Int -> A.Acc (A.Vector A.DIM1)
        writes inc = A.zipWith3 (f inc) headFlags startIndex countSmaller
      in
        -- Note that (writes 1) may go out of bounds of the values array.
        -- We made the headFlags array one larger, such that this gives no problems.
        writeFlags (writes 0) $ writeFlags (writes 1) $ headFlags

-- Checks whether all segments have length 1. If that is the case, then the loop may terminate.
condition :: A.Acc State -> A.Acc (A.Scalar Bool)
condition (A.T2 _ headFlags) = A.map A.not $ A.fold (A.&&) (A.constant True) headFlags

-- Finds the new index of an element of the list, as the result of the partition
partitionPermuteIndex :: A.Exp Bool -> A.Exp Int -> A.Exp Int -> A.Exp Int -> A.Exp Int -> A.Exp Int
partitionPermuteIndex isLarger start indexIfSmaller indexIfLarger countSmaller =
  start + (isLarger A.? (countSmaller + indexIfLarger, indexIfSmaller))

-- Given head flags, propagates the value of the head to all elements in the segment
propagateSegmentHead :: A.Elt a => A.Acc (A.Vector Bool) -> A.Acc (A.Vector a) -> A.Acc (A.Vector a)
propagateSegmentHead headFlags values
  = A.map A.fst
  $ A.postscanl f (A.T2 A.undef $ A.constant True)
  $ A.zip values headFlags
  where
    f left (A.T2 rightValue rightFlag) =
      A.cond rightFlag (A.T2 rightValue $ A.constant True) left

-- Given head flags, propagates the value of the head to all elements in the segment
propagateSegmentLast :: A.Elt a => A.Acc (A.Vector Bool) -> A.Acc (A.Vector a) -> A.Acc (A.Vector a)
propagateSegmentLast headFlags values
  = A.map A.fst
  $ A.postscanr f (A.T2 A.undef $ A.constant True)
  $ A.zip values 
  $ A.tail headFlags
  where
    f (A.T2 leftValue leftFlag) right =
      A.cond leftFlag (A.T2 leftValue $ A.constant True) right

-- Segmented postscan, where the segments are defined with head flags
postscanSegHead :: A.Elt a => (A.Exp a -> A.Exp a -> A.Exp a) -> A.Acc (A.Vector Bool) -> A.Acc (A.Vector a) -> A.Acc (A.Vector a)
postscanSegHead f headFlags values
  = A.map A.fst
  $ A.postscanl g (A.T2 A.undef $ A.constant True)
  $ A.zip values headFlags
  where
    g (A.T2 leftValue leftFlag) (A.T2 rightValue rightFlag)
      = A.T2
          (rightFlag A.? (rightValue, f leftValue rightValue))
          (leftFlag A..|. rightFlag)

-- Writes True to the specified indices in a flags arrays
writeFlags :: A.Acc (A.Vector A.DIM1) -> A.Acc (A.Vector Bool) -> A.Acc (A.Vector Bool)
writeFlags writes flags = A.permute const flags (writes A.!) (A.fill (A.shape writes) $ A.constant True)
