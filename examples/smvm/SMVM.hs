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
        offs            = A.scanl (+) 0 segd

        smat' :: Seq [SparseVector a]
        smat' = produce (A.length segd) getRow

        getRow :: Acc (Scalar Int) -> Acc (SparseVector a)
        getRow i = generate (index1 (A.fromIntegral (segd A.!! the i)))
                            (\ix -> get (the i) (unindex1 ix))

        get :: Exp Int -> Exp Int -> Exp (Int32, a)
        get row ix =
          let
            ix' = A.fromIntegral (offs A.!! row) + ix
          in svec A.!! ix'

        sdotp :: Acc (SparseVector a) -> Acc (Vector a) -> Acc (Scalar a)
        sdotp xs ys = let (inds, vals) = A.unzip xs
                      in fold (+) 0
                       $ A.zipWith (*) vals (gather (A.map A.fromIntegral inds) ys)

    in collect . elements $ mapSeq (flip sdotp vec) smat'
