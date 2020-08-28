{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QuickSort (quicksort) where

import Data.Array.Accelerate
import Data.Array.Accelerate.Unsafe
import Data.Array.Accelerate.Data.Bits
import Data.Array.Accelerate.Examples.Internal


quicksort :: Backend -> Vector Int -> Vector Int
quicksort backend = runN backend quicksort'

quicksort' :: Ord a => Acc (Vector a) -> Acc (Vector a)
quicksort' input = result
  where
    -- Initially, we have one segment, namely the whole array
    initialFlags = scatter (fill (I1 1) 0 ++ fill (I1 1) (length input)) emptyFlags fullFlags
    emptyFlags   = fill (I1 (1 + length input)) False_
    fullFlags    = fill (I1 2) True_

    -- We stop when each segment contains just one element, as segments of
    -- one element are sorted.
    T2 result _ = awhile condition step $ T2 input initialFlags

type State a =
  ( Vector a      -- Values
  , Vector Bool   -- Head flags, denoting the starting points of the unsorted segments
  )

step :: Ord a => Acc (State a) -> Acc (State a)
step (T2 values headFlags) = (T2 values' headFlags')
  where
    -- Per element, the pivot of the segment of that element
    -- For each segment, we just take the first element as pivot
    pivots = propagateSegmentHead headFlags values

    -- Find which elements are larger than the pivot
    isLarger = zipWith (>=) values pivots

    -- Propagate the start index of a segment to all elements
    startIndex = propagateSegmentHead headFlags (generate (shape values) unindex1)

    -- Compute the offsets to which the elements must be moved using a scan
    indicesLarger, indicesSmaller :: Acc (Vector Int)
    indicesLarger  = map (\x -> x - 1) $ postscanSegHead (+) headFlags $ map (? (1, 0)) isLarger
    indicesSmaller = map (\x -> x - 1) $ postscanSegHead (+) headFlags $ map (? (0, 1)) isLarger

    -- Propagate the number of smaller elements to each segment
    -- This is needed as an offset for the larger elements
    countSmaller :: Acc (Vector Int)
    countSmaller = map (+1) $ propagateSegmentLast headFlags indicesSmaller

    -- Compute the new indices of the elements
    permutation = zipWith5 partitionPermuteIndex isLarger startIndex indicesSmaller indicesLarger countSmaller

    -- Perform the permutation
    values' = scatter permutation (fill (shape values) undef) values

    -- Update the head flags for the next iteration (the 'recursive call' in a traditional implementation)
    -- Mark new section starts at:
    --  * the position of the pivot
    --  * the position of the pivot + 1
    headFlags' =
      let
          f :: Int -> Exp Bool -> Exp Int -> Exp Int -> Exp (Maybe DIM1)
          f inc headF start countSmall =
            headF ? (Just_ (I1 $ start + countSmall + constant inc), Nothing_)

          writes :: Int -> Acc (Vector (Maybe DIM1))
          writes inc = zipWith3 (f inc) headFlags startIndex countSmaller
      in
      -- Note that (writes 1) may go out of bounds of the values array.
      -- We made the headFlags array one larger, such that this gives no problems.
      writeFlags (writes 0) $ writeFlags (writes 1) $ headFlags

-- Checks whether all segments have length 1. If that is the case, then the
-- loop may terminate.
--
condition :: Elt a => Acc (State a) -> Acc (Scalar Bool)
condition (T2 _ headFlags) = map not $ fold (&&) True_ headFlags

-- Finds the new index of an element of the list, as the result of the
-- partition
--
partitionPermuteIndex :: Exp Bool -> Exp Int -> Exp Int -> Exp Int -> Exp Int -> Exp Int
partitionPermuteIndex isLarger start indexIfSmaller indexIfLarger countSmaller =
  start + (isLarger ? (countSmaller + indexIfLarger, indexIfSmaller))

-- Given head flags, propagates the value of the head to all elements in
-- the segment
--
propagateSegmentHead
    :: Elt a
    => Acc (Vector Bool)
    -> Acc (Vector a)
    -> Acc (Vector a)
propagateSegmentHead headFlags values
  = map fst
  $ postscanl f (T2 undef True_)
  $ zip values headFlags
  where
    f left (T2 rightValue rightFlag) =
      if rightFlag
         then T2 rightValue True_
         else left

-- Given head flags, propagates the value of the head to all elements in
-- the segment
--
propagateSegmentLast
    :: Elt a
    => Acc (Vector Bool)
    -> Acc (Vector a)
    -> Acc (Vector a)
propagateSegmentLast headFlags values
  = map fst
  $ postscanr f (T2 undef True_)
  $ zip values
  $ tail headFlags
  where
    f (T2 leftValue leftFlag) right =
      if leftFlag
         then T2 leftValue True_
         else right

-- Segmented postscan, where the segments are defined with head flags
--
postscanSegHead
    :: Elt a
    => (Exp a -> Exp a -> Exp a)
    -> Acc (Vector Bool)
    -> Acc (Vector a)
    -> Acc (Vector a)
postscanSegHead f headFlags values
  = map fst
  $ postscanl g (T2 undef True_)
  $ zip values headFlags
  where
    g (T2 leftValue leftFlag) (T2 rightValue rightFlag)
      = T2
          (rightFlag ? (rightValue, f leftValue rightValue))
          (leftFlag .|. rightFlag)

-- Writes True to the specified indices in a flags arrays
--
writeFlags
    :: Acc (Vector (Maybe DIM1))
    -> Acc (Vector Bool)
    -> Acc (Vector Bool)
writeFlags writes flags = permute const flags (writes !) (fill (shape writes) True_)

