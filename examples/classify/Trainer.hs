{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}
module Trainer where

import Control.Exception                                ( evaluate )
import Control.Monad
import Criterion.Measurement                            ( getTime, secs, initializeTime )
import Data.Array.Accelerate                            hiding ( tail, init, length, (++), fst, max )
import Data.Array.Accelerate.Examples.Internal
import Prelude                                          hiding ( map, fromIntegral, last, zipWith, zipWith3, last, uncurry, replicate, Eq(..) )
import System.Mem

import Image
import Network

vectorResult :: Acc (Scalar Word8) -> Acc (Vector Float)
vectorResult r = generate (index1 10) (\ix -> unindex1 ix == fromIntegral (the r) ? (1.0, 0.0))

flattenGradients :: Acc (Layer, Layer)
                 -> Seq [( (Vector Float, Vector Float)
                         , (Matrix Float, Matrix Float) )]
                 -> Seq ( (Layer, Layer)
                        , (Matrix Float, Matrix Float)
                        , (Array DIM3 Float, Array DIM3 Float) )
flattenGradients n dn =
  let
    (nabla_bs, nabla_ws) = unzipSeq dn
    (db1, db2)           = unzipSeq nabla_bs
    (dw1, dw2)           = unzipSeq nabla_ws
  in lift ( n, (tabulate db1, tabulate db2), (tabulate dw1, tabulate dw2) )

batch3 :: Exp Float
       -> Acc ( (Layer, Layer),
                (Matrix Float, Matrix Float),
                (Array DIM3 Float, Array DIM3 Float) )
       -> Acc (Layer, Layer)
batch3 eta (unlift -> (n,db,dw) :: ( Acc (Layer, Layer), Acc (Matrix Float, Matrix Float), Acc (Array DIM3 Float, Array DIM3 Float) )) =
  let
    (l1,l2) = unlift n
    (db1,db2) = unlift db
    (dw1,dw2) = unlift dw
    batchSize = indexHead (indexTrans (shape db1))
    gd x nx = x - (eta / fromIntegral batchSize) * nx

    combine :: (Shape sh, Slice sh) => Acc (Array sh Float) -> Acc (Array (sh:.Int) Float) -> Acc (Array sh Float)
    combine a = zipWith gd a
              . transpose
              . fold (+) 0
              . transpose

    b1 = combine (biases l1) db1
    b2 = combine (biases l2) db2
    w1 = combine (weights l1) dw1
    w2 = combine (weights l2) dw2

  in lift ((b1,w1), (b2,w2))

train3 :: Backend -> Float -> Array DIM3 Word8 -> Vector Word8 -> IO (Layer, Layer)
train3 backend eta input output = do
  let Z:._:.h:.w = arrayShape input

  let !trainer = run1 backend (train3' eta input output)

  -- Run one epoch of the trainer in order to force frontend conversion
  --
  [l1,l2] <- randomLayers [h*w,1,10]
  trainer (l1,l2) `seq` return ()
  performGC
  putStrLn "Front-end conversion completed"
  --

  -- From criterion, to use their timing mechanism.
  [l1',l2'] <- randomLayers [h*w,300,10]
  initializeTime
  (time, layers) <- measure (evaluate $! (trainer (l1',l2')))
  putStrLn ("Epoch in " ++ secs time)
  return layers

train3' :: Float
        -> Array DIM3 Word8
        -> Vector Word8
        -> Acc (Layer, Layer)
        -- -> Acc (Scalar Bool)
        -> Acc (Layer, Layer)
train3' eta input output initialNetwork =
  let images         = allImages input
      labels         = mapSeq (vectorResult . reshape index0) $ subarrays (index1 1) output
  in collect $ foldBatch (\n (unzipSeq -> (x,y)) -> flattenGradients n (zipWithSeq (backprop3 n) x y))
                         (batch3 (lift eta))
                         initialNetwork
             $ zipSeq images labels

reshape' :: (Shape sh, Slice sh, Elt e) => Exp sh -> Acc (Vector e) -> Acc (Array (sh:.Int) e)
reshape' sh a = reshape (indexSnoc (size a `div` shapeSize sh) sh) a
  where
    indexSnoc :: (Shape sh, Slice sh) => Exp Int -> Exp sh -> Exp (sh:.Int)
    indexSnoc i = indexTrans . lift . (:. i) . indexTrans

measure :: IO b
        -> IO (Double, b)
measure action = do
  startTime <- getTime
  !b <- action
  endTime <- getTime
  return (max 0 (endTime - startTime), b)
