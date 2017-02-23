
module Time where

import Text.Printf
import Data.Time.Clock
import System.CPUTime
import Data.Array.Accelerate.Examples.Internal


timed :: IO a -> IO a
timed action = do
  wall0 <- getCurrentTime
  cpu0  <- getCPUTime
  res   <- action
  wall1 <- getCurrentTime
  cpu1  <- getCPUTime
  --
  let wallTime = realToFrac (diffUTCTime wall1 wall0)
      cpuTime  = fromIntegral (cpu1 - cpu0) * 1E-12
  --
  printf (elapsed wallTime cpuTime)
  return res

elapsed :: Double -> Double -> String
elapsed wallTime cpuTime =
  printf "  elapsed    : %s (wall), %s (cpu), %.2f x speedup"
    (showFFloatSIBase (Just 3) 1000 wallTime "s")
    (showFFloatSIBase (Just 3) 1000 cpuTime  "s")
    (cpuTime / wallTime)

