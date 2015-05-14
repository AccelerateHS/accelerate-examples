{-# LANGUAGE BangPatterns #-}
import MVM
import Matrix
import Config

import Prelude                                          as P
import Data.Functor
import Data.Label                                       ( get )
import Data.Array.Accelerate                            as A
import qualified Data.Array.Accelerate.CUDA                       as CUDA
import Data.Array.Accelerate.Examples.Internal          as A


main :: IO ()
main = do
  beginMonitoring

  (conf, opts, rest)       <- parseArgs options defaults header footer

  -- Generate matrix and vector
  --
  let rows = get configRows conf
      cols = get configCols conf

  mat     <- randomMatrix A.uniform rows cols   :: IO (Matrix Float)
  vec     <- randomVector A.uniform cols        :: IO (Vector Float)
  bigvec1 <- randomVector A.uniform (cols*rows) :: IO (Vector Float)
  bigvec2 <- randomVector A.uniform (cols*rows) :: IO (Vector Float)

  let backend = get optBackend opts

  -- Acclerate by default keeps arrays in memory till they can be used again. As
  -- we are wanting to test streaming results, obviously data transfer is a big
  -- part of that, so we want to make sure all arrays are removed from the cache
  -- for every benchmark iteration.
  --
  let clean = do
                CUDA.unsafeFree mat
                CUDA.unsafeFree vec
                CUDA.unsafeFree bigvec1
                CUDA.unsafeFree bigvec2

  let bench' name f = runBenchmarks opts rest [bench name f]

  let !mvm'        = run2 backend mvm
      !mvmSeqLazy  = run1 backend (mvmSeq (use mat))
      !mvmSeq'     = run2 backend mvmSeq
      !dotp'       = run2 backend dotp
      !dotpSeqLazy = run1 backend (\a -> lift (a, dotpSeq (use bigvec1) (use bigvec2)))
      !dotpSeq'    = run2 backend dotpSeq

  let force0 arr = indexArray arr Z `seq` arr
  let force1 arr = indexArray arr (Z:.0) `seq` arr

  -- Benchmark
  --
  bench' "mvm"               $ whnfIO (force1 <$> runClean (mvm' mat) vec clean)
  bench' "mvmSeq-uselazy"    $ whnfIO (force1 <$> runClean mvmSeqLazy vec clean)
  bench' "mvmSeq-nouselazy"  $ whnfIO (force1 <$> runClean (mvmSeq' mat) vec clean)
  bench' "dotp"              $ whnfIO (force0 <$> runClean (dotp' bigvec1) bigvec2 clean)
  bench' "dotpSeq-uselazy"   $ whnfIO (force0 . P.snd <$> runClean dotpSeqLazy () clean)
  bench' "dotpSeq-nouselazy" $ whnfIO (force0 <$> runClean (dotpSeq' bigvec1) bigvec2 clean)

-- Run a function with the given teardown action afterward.
{-# NOINLINE runClean #-}
runClean :: (a -> b) -> a -> IO () -> IO b
runClean f a clean = do
  clean
  f <$> return a
