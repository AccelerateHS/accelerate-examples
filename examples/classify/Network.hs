{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ViewPatterns        #-}

module Network where

import Data.Array.Accelerate            as A

import Prelude                          as P
import Data.List                        ( mapAccumL )

-- Stochastic gradient descent learning for a neural network.
-- ----------------------------------------------------------
--
-- This is largely based on the python implementation from the book "Neural
-- Networks and Deep Learning" by Michael Neilsen from
-- http://neuralnetworksanddeeplearning.com/
--
-- The python code itself can be seen here:
-- https://github.com/mnielsen/neural-networks-and-deep-learning/blob/master/src/network.py
--

type Layer = ( Vector Float    -- Biases
             , Matrix Float)   -- Weights

type Matrix = Array DIM2

biases :: Acc Layer -> Acc (Vector Float)
biases = afst

weights :: Acc Layer -> Acc (Matrix Float)
weights = asnd

sigmoid :: Exp Float -> Exp Float
sigmoid z = 1 / (1 + exp z)

sigmoid' :: Exp Float -> Exp Float
sigmoid' z = sigmoid z * (1 - sigmoid z)

feedforward :: [Acc Layer] -> Acc (Vector Float) -> Acc (Vector Float)
feedforward layers x = foldl f x layers
  where
    f a l = A.map sigmoid (A.zipWith (+) (mvm (weights l) a) (biases l))

backprop :: [Acc Layer] -> Acc (Vector Float) -> Acc (Vector Float) -> ([Acc (Vector Float)], [Acc (Matrix Float)])
backprop layers x y = (nabla_b, nabla_w)
  where
    -- Forward pass
    zs :: [Acc (Vector Float)]
    activations :: [Acc (Vector Float)]
    (zs, activations) = P.unzip . P.snd
        $ mapAccumL (\a l -> let z = A.zipWith (+) (mvm (weights l) a) (biases l)
                                 a' = A.map sigmoid z
                             in (a', (z,a')))
                    x
                    layers

    -- Backward pass
    nabla_b_final = A.zipWith (*) (costDerivative (P.last activations) y) (A.map sigmoid' (P.last zs))
    nabla_w_final = cross nabla_b_final (P.last activations)

    nabla_b :: [Acc (Vector Float)]
    nabla_w :: [Acc (Matrix Float)]
    (nabla_b, nabla_w) = P.unzip . P.scanr (\(z,w,a) (delta,_) ->
                           let sp     = A.map sigmoid' z
                               delta' = A.zipWith (*) (mvm (transpose w) delta) sp
                           in (delta', cross delta' a))
                           (nabla_b_final, nabla_w_final)
                       $ P.zip3 (P.init $ zs)
                                (P.init . P.map weights $ layers)
                                (x : activations)

costDerivative :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Vector Float)
costDerivative outputActivation y = A.zipWith (-) outputActivation y

mvm :: (Elt a, IsNum a) => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
mvm mat vec
  = let h = A.fst (unindex2 (shape mat))
    in A.fold (+) 0 $ A.zipWith (*) mat (A.replicate (A.lift (Z:.h:.All)) vec)

cross :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Matrix Float)
cross v h = A.zipWith (*) (A.replicate (lift (Z:.All:.size h)) v) (A.replicate (lift (Z:.size v:.All)) h)
