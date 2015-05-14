
import MVM
import Matrix
import Config

import Prelude                                          as P
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

  let mat     = randomMatrix A.uniform rows cols   :: Matrix Float
      vec     = randomVector A.uniform cols        :: Vector Float
      bigvec1 = randomVector A.uniform (cols*rows) :: Vector Float
      bigvec2 = randomVector A.uniform (cols*rows) :: Vector Float

  let backend   = get optBackend opts

  let bench' name f = runBenchmarks opts rest [bench name f] >> CUDA.performGC

  -- Benchmark
  --
  bench' "mvm"              $ whnf (run2 backend mvm mat) vec
  bench' "mvmSeq-uselazy"   $ whnf (run1 backend (mvmSeq (use mat))) vec
  bench' "mvmSeq-nouselazy" $ whnf (run2 backend mvmSeq mat) vec
  bench' "dotp"             $ whnf (run2 backend dotp bigvec1) bigvec2
  bench' "dotpSeq"          $ whnf (run1 backend (\_ -> dotpSeq (use bigvec1) (use bigvec2))) ()

