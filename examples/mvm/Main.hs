
import MVM
import Matrix
import Config

import Prelude                                          as P
import Data.Label                                       ( get )
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Examples.Internal          as A


main :: IO ()
main = do
  beginMonitoring

  (conf, opts, rest)       <- parseArgs options defaults header footer

  -- Generate matrix and vector
  --
  let rows = get configRows conf
      cols = get configCols conf

  let mat = randomMatrix A.uniform rows cols :: Matrix Float
      vec = randomVector A.uniform cols      :: Vector Float

  let backend   = get optBackend opts

  -- Benchmark
  --
  runBenchmarks opts rest
    [ bench "mvm" $ whnf (run2 backend mvm mat) vec
    , bench "mvmSeq" $ whnf (run2 backend mvmSeq mat) vec]

