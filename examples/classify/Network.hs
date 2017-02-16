{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Network (

  Layer, Matrix, biases, weights, feedforward, backprop, backprop3,

  randomLayers,

) where

import Data.Array.Accelerate            as A
import Data.Array.Accelerate.Examples.Internal

import Prelude                          as P
import Control.Monad
import Data.List                        ( mapAccumL )
import GHC.Float

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
sigmoid z = 1 / (1 + exp (-z))

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
    nabla_w_final = cross nabla_b_final (P.last (P.init activations))

    nabla_b :: [Acc (Vector Float)]
    nabla_w :: [Acc (Matrix Float)]
    (nabla_b, nabla_w) = P.unzip . P.scanr (\(z,w,a) (delta,_) ->
                           let sp     = A.map sigmoid' z
                               delta' = A.zipWith (*) (mvm (transpose w) delta) sp
                           in (delta', cross delta' a))
                           (nabla_b_final, nabla_w_final)
                       $ P.zip3 (P.init $ zs)
                                (P.tail . P.map weights $ layers)
                                (x : activations)

costDerivative :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Vector Float)
costDerivative outputActivation y = A.zipWith (-) outputActivation y

mvm :: (Elt a, A.Num a) => Acc (Matrix a) -> Acc (Vector a) -> Acc (Vector a)
mvm mat vec
  = let Z:.h:._ = unlift (shape mat) :: Z:.Exp Int:.Exp Int
    in A.fold (+) 0 $ A.zipWith (*) mat (A.replicate (A.lift (Z:.h:.All)) vec)

cross :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Matrix Float)
cross v h = A.zipWith (*) (A.replicate (lift (Z:.All:.size h)) v) (A.replicate (lift (Z:.size v:.All)) h)

randomBiases :: Int -> IO (Vector Float)
randomBiases = randomArrayIO standardFloat . (Z:.) -- TODO: Configurable random generators

randomWeights :: DIM2 -> IO (Array DIM2 Float)
randomWeights = randomArrayIO standardFloat

randomLayers :: [Int] -> IO [Layer]
randomLayers sizes = zipWithM f (P.init sizes) (P.tail sizes)
  where
    f x y = (,) <$> randomBiases y <*> randomWeights (Z:.y:.x)

standardFloat :: Shape sh => sh :~> Float
standardFloat sh gen = double2Float <$> standard sh gen

backprop3 :: Acc (Layer, Layer)
          -> Acc (Vector Float)
          -> Acc (Vector Float)
          -> Acc ((Vector Float, Vector Float)
                 ,(Matrix Float, Matrix Float))
backprop3 (unlift -> (l1,l2)) x y =
  let ([db1,db2], [dw1,dw2]) = backprop [l1,l2] x y
  in lift ((db1,db2), (dw1,dw2))
