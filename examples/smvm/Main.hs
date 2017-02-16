
import SMVM
import Matrix
import Config

import Prelude                                          as P
import Data.Label                                       ( get )
import System.Random.MWC
import System.Exit
import System.Environment
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Examples.Internal          as A
import qualified Data.Vector.Unboxed                    as V


main :: IO ()
main = withSystemRandom $ \gen -> do
  beginMonitoring

  (_, opts, rest)       <- parseArgs options defaults header footer
  fileIn                <- case rest of
    (i:_)       -> return i
    _           -> withArgs ["--help"] $ parseArgs options defaults [] []
                >> exitSuccess

  -- Read in the matrix file, and generate a random vector to multiply against
  --
  (segd', svec', cols) <- readCSRMatrix gen fileIn
  vec'                 <- uniformVector gen cols

  -- Convert to Accelerate arrays
  --
  let backend   = get optBackend opts

      vec       = fromFunction (Z :. V.length vec')  (\(Z:.i) -> vec'  V.! i)
      segd      = fromFunction (Z :. V.length segd') (\(Z:.i) -> segd' V.! i)
      svec      = fromFunction (Z :. V.length svec') (\(Z:.i) -> svec' V.! i)
      smat      = lift (use segd, svec)

      smat'     = streamIn $ go 0 0
        where
          go seg offset
            | seg P.>= V.length segd' = []
            | otherwise               = fromFunction (Z:.n) (\(Z:.i) -> svec' V.! (offset+i)) : go (seg+1) (offset+n)
                where n = P.fromIntegral (segd' V.! seg)

  putStrLn $ "Reading matrix: " P.++ fileIn
  putStrLn $ "  with shape: " P.++ shows (V.length segd') " x " P.++ shows cols " and "
                              P.++ shows (V.length svec') " entries\n"

  -- Benchmark
  --
  runBenchmarks opts (P.tail rest)
    [ bgroup "smvm"
      [ bench "foldSeg"   $ whnf (run1 backend (smvm smat))    vec
      , bgroup "sequences"
        [ bench "fromShapes" $ whnf (run1 backend (smvmSeq  smat)) vec
        , bench "streamIn"   $ whnf (run1 backend (smvmSeq' smat')) vec
        ]
      ]
    ]

