{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Config
import QuickSort

import Control.Monad
import Data.Array.IO
import Data.Label
import Data.Maybe
import System.Random
import Text.Read (readMaybe)

import Data.Array.Accelerate.Examples.Internal
import qualified Data.Array.Accelerate                              as A

main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest) <- parseArgs options defaults header footer
  let qsort = quicksort $ get optBackend opts

  case (get configRandomSize conf, rest) of
    (Nothing, [path]) -> do
      -- Read input from file
      file <- readFile path
      let input = map (fromMaybe (error "Parse error. Input file should contain integers separated by spaces") . readMaybe) $ words file
      let result = A.toList $ qsort $ A.fromList (A.Z A.:. length input) input
      putStrLn $ unwords $ map show result
    (Just size, _) -> do
      -- Do multiple runs using a random input
      -- For later runs, the program does not have to be recompiled
      replicateM_ 10 $ randomRun qsort size
    _ -> do
      putStrLn "Usage: accelerate-quicksort input.txt"
      putStrLn "Or:    accelerate-quicksort --size 1000"

randomRun :: (A.Vector Int -> A.Vector Int) -> Int -> IO ()
randomRun qsort size = do
  putStrLn $ "Sorting " ++ show size ++ " elements"
  input <- shuffle [1 .. size]
  let result = A.toList $ qsort $ A.fromList (A.Z A.:. length input) input
  putStrLn $ unwords (map show $ take 30 result) ++ (if size <= 30 then "" else " ...")
  putStrLn ""

-- | /O(N)/ Randomly shuffle a list
-- From https://wiki.haskell.org/Random_shuffle
--
shuffle :: forall a. [a] -> IO [a]
shuffle xs = do
  let n = length xs
  ar <- newListArray (1,n) xs :: IO (IOArray Int a)
  forM [1..n] $ \i -> do
      j <- randomRIO (i,n)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj

