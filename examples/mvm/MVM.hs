{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module MVM where

import Matrix
import Data.Array.Accelerate            as A

mvm :: (Elt a, IsNum a) => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
mvm mat vec
  = let h = A.fst (unindex2 (shape mat))
    in A.fold (+) 0 $ A.zipWith (*) mat (A.replicate (A.lift (Z:.h:.All)) vec)

mvmSeq :: (Elt a, IsNum a) => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
mvmSeq mat vec
  = let rows     = toSeqOuter mat
    in collect (concatElems (mapSeq (dotp vec) rows))

dotp :: (Elt a, IsNum a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Scalar a)
dotp a b = A.fold (+) 0 (A.zipWith (*) a b)

dotpSeq :: (Elt a, IsNum a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Scalar a)
dotpSeq xs ys = A.collect $ A.foldSeqE (+) 0 $ A.zipWithSeq (A.zipWith (*)) (toSeqOuter xs) (toSeqOuter ys)

maxSumSeq :: (Elt a, IsNum a) => Acc (Vector a) -> Acc (Scalar (a, a))
maxSumSeq xs = let xs' = toSeqOuter xs in (\(unlift -> (m,s)) -> unit $ lift (the m, the s)) . A.collect $ lift $ (foldSeqE max 0 xs', foldSeqE (+) 0 xs')

maxSum :: (Elt a, IsNum a) => Acc (Vector a) -> Acc (Scalar (a, a))
maxSum xs = unit $ lift (the (fold max 0 xs), the (fold (+) 0 xs))
