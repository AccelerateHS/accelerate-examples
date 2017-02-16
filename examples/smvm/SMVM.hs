{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SMVM where

import Data.Array.Accelerate            as A


-- Sparse-matrix vector multiplication
-- -----------------------------------

type SparseVector e = Vector (Int32, e)
type SparseMatrix e = (Segments Int32, SparseVector e)


smvm :: A.Num a => Acc (SparseMatrix a) -> Acc (Vector a) -> Acc (Vector a)
smvm smat vec
  = let (segd, svec)    = unlift smat
        (inds, vals)    = A.unzip svec

        vecVals         = gather (A.map A.fromIntegral inds) vec
        products        = A.zipWith (*) vecVals vals
    in
    foldSeg (+) 0 products segd

smvmSeq :: forall a. A.Num a => Acc (SparseMatrix a) -> Acc (Vector a) -> Acc (Vector a)
smvmSeq smat vec
  = let (segd, svec)    = unlift smat :: (Acc (Segments Int32), Acc (SparseVector a))

        smat' :: Seq [SparseVector a]
        smat' = fromShapes (A.map (index1 . A.fromIntegral) segd) svec

        sdotp :: Acc (SparseVector a) -> Acc (Vector a) -> Acc (Scalar a)
        sdotp xs ys = let (inds, vals) = A.unzip xs
                      in fold (+) 0
                       $ A.zipWith (*) vals (gather (A.map A.fromIntegral inds) ys)

    in collect . elements $ mapSeq (flip sdotp vec) smat'

smvmSeq' :: A.Num a => Seq [SparseVector a] -> Acc (Vector a) -> Acc (Vector a)
smvmSeq' smat vec
  = let dot xs ys = A.fold (+) 0 ( A.zipWith (*) xs ys )
        sdot srow = let (ix,xs) = A.unzip srow
                    in  dot xs (gather (A.map A.fromIntegral ix) vec)
    in collect
     $ elements
     $ mapSeq sdot smat

