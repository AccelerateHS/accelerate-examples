{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MVM where

import Matrix
import Data.Array.Accelerate            as A

mvm :: (Elt a, IsNum a) => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
mvm mat vec
  = let h = A.fst (unindex2 (shape mat))
    in A.fold (+) 0 $ A.zipWith (*) mat (A.replicate (A.lift (Z:.h:.All)) vec)

mvmSeq :: (Elt a, IsNum a) => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
mvmSeq mat vec
  = let rows     = toSeqOuter2 mat
    in A.collect (A.fromSeqElems (mapSeq (dotp vec) rows))

dotp :: (Elt a, IsNum a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Scalar a)
dotp a b = A.fold (+) 0 (A.zipWith (*) a b)

dotpSeq :: (Elt a, IsNum a) => Acc (Vector a) -> Acc (Vector a) -> Acc (Scalar a)
dotpSeq xs ys = A.collect $ A.foldSeq (+) 0 $ A.zipWithSeq (tlift2 (*)) (toSeqElems xs) (toSeqElems ys)
  where
    tlift2 :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> Exp c) -> (Acc (Scalar a) -> Acc (Scalar b) -> Acc (Scalar c))
    tlift2 f a b = unit (f (the a) (the b))

    toSeqElems = toSeq (Z:.Split)
