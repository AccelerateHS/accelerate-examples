{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
import MVM
import Config                                           hiding ( Config )

import Prelude                                          as P
import Control.Exception                                ( evaluate )
import Data.Functor
import Data.Label                                       ( get )
import System.Environment
import System.Random
import Criterion.Measurement                            ( getTime, secs, initializeTime )
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Examples.Internal          as A hiding ( bench )
import qualified Data.Array.Accelerate.CUDA             as CUDA
import qualified Data.Vector                            as V


main :: IO ()
main = do
  beginMonitoring

  (conf, opts, rest)       <- parseArgs options defaults header footer

  -- Generate matrix and vector
  --
  let rows = get configRows conf
      cols = get configCols conf

  let backend  = get optBackend opts
      iters    = get configIters conf
      hostArrs = get configHostArrays conf

  let bench name f tearups = do
        putStrLn ("Benchmarking " P.++ name P.++ ".")
        ms <- runBenchmark f tearups (\a -> CUDA.unsafeFree a >> CUDA.performGC)
        putStrLn ("Analysing results...")
        let sz   = V.length ms
        let mean = V.sum ms / P.fromIntegral sz
        let lb   = V.foldl1 max ms
        let ub   = V.foldl1 min ms
        let var  = V.sum (V.map ((**2) . (mean -)) ms) / P.fromIntegral sz
        let sd   = sqrt var
        putStrLn ("mean: " P.++ secs mean P.++ " (" P.++ secs lb P.++ " .. " P.++ secs ub P.++ ")")
        putStrLn ("standard deviation: " P.++ secs sd)


  -- Converted accelerate computations. Have to be careful we're not accidently
  -- including compilation time in useLazy benchmarks.
  --
  let !mvm'              = run2 backend mvm
      !mvmSeqLazy        = run1 backend . mvmSeq . use
      !mvmSeq'           = run2 backend mvmSeq
      !dotp'             = run2 backend dotp
      dotpSeqLazy xs ys  = run1 backend (\a -> lift (a, dotpSeq (use xs) (use ys)))
      !dotpSeq'          = run2 backend dotpSeq
      !maxSum'           = run1 backend maxSum
      maxSumSeqLazy xs   = run1 backend (\a -> lift (a, maxSumSeq (use xs)))
      !maxSumSeq'        = run1 backend maxSumSeq

  -- Need to make sure we force the results of computations.
  --
  let force0 arr = indexArray arr Z `seq` arr
      force1 arr = indexArray arr (Z:.0) `seq` arr
      force2 arr = indexArray arr (Z:.0:.0) `seq` arr

  -- Poor man's random array generation
  --
  let genMat :: Scalar Float -> Array DIM2 Float
      genMat = if hostArrs then fromFunction (Z:.rows:.cols) . const . flip indexArray Z
                           else run1 backend (A.fill (lift (Z:.rows:.cols)) . the . lift)

  let genVec :: Scalar Int -> Scalar Float -> Array DIM1 Float
      genVec sz = if hostArrs then fromFunction (Z :. (sz `indexArray` Z)) . const . flip indexArray Z
                              else run1 backend (A.fill (index1 (the (lift sz))) . the . lift)

  let matsVecs = P.replicate iters $ do
               i <- randomIO :: IO Float
               let m = genMat (fromList Z [i])
               let v = genVec (fromList Z [cols]) (fromList Z [i])
               _ <- evaluate (force1 v)
               _ <- evaluate (force2 m)
               CUDA.unsafeFree m
               CUDA.unsafeFree v
               CUDA.performGC
               return (m,v)

  let vecsVecs = P.replicate iters $ do
               i1 <- randomIO :: IO Float
               i2 <- randomIO :: IO Float
               let v1 = genVec (fromList Z [rows*cols]) (fromList Z [i1])
                   v2 = genVec (fromList Z [rows*cols]) (fromList Z [i2])
               _ <- evaluate (force1 v1)
               _ <- evaluate (force1 v2)
               CUDA.unsafeFree v1
               CUDA.unsafeFree v2
               CUDA.performGC
               return (v1,v2)

  let vecs = P.replicate iters $ do
               i1 <- randomIO :: IO Float
               let v1 = genVec (fromList Z [rows*cols]) (fromList Z [i1])
               _ <- evaluate (force1 v1)
               CUDA.unsafeFree v1
               CUDA.performGC
               return v1

  -- From criterion, to use their timing mechanism.
  initializeTime

  -- Benchmark
  --
  withArgs rest $ do
    bench "mvm"                 (force1 . P.uncurry mvm')         matsVecs
    bench "mvmSeq-uselazy"      (\(f,b) -> force1 (f b) )         (P.map ((\(a,b) -> (mvmSeqLazy a, b)) <$>) matsVecs)
    bench "mvmSeq-nouselazy"    (force1 . P.uncurry mvmSeq')      matsVecs
    bench "dotp"                (force0 . P.uncurry dotp')        vecsVecs
    bench "dotpSeq-uselazy"     (\(f,b) -> force0 (P.snd (f b)))  (P.map ((\(a,b) -> (flip (dotpSeqLazy a) (), b)) <$>) vecsVecs)
    bench "dotpSeq-nouselazy"   (force0 . P.uncurry dotpSeq')     vecsVecs
    bench "maxSum"              (force0 . maxSum')                vecs
    bench "maxSumSeq-uselazy"   (\(f,b) -> force0 (P.snd (f b)) ) (P.map ((\a -> (flip maxSumSeqLazy (), a)) <$>) vecs)
    bench "maxSumSeq-nouselazy" (force0 . maxSumSeq')             vecs

-- Benchmark a function many times over a list of tearups and a teardown.
--
{-# NOINLINE runBenchmark #-}
runBenchmark :: forall a b. (a -> b) -> [IO a] -> (b -> IO ()) -> IO (V.Vector Double)
runBenchmark f tearups teardown = do
  putStrLn ("Running " P.++ show (P.length tearups) P.++ " iterations.")
  ms <- mapM runIteration tearups
  return $! (V.fromList (P.drop 10 ms))
  where
    runIteration :: IO a -> IO Double
    runIteration tearup = do
      a <- tearup
      (m,b) <- measure (evaluate (f a))
      teardown b
      return m

measure :: IO b
        -> IO (Double, b)
measure action = do
  startTime <- getTime
  !b <- action
  endTime <- getTime
  return (max 0 (endTime - startTime), b)
