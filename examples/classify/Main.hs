{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Config                                           hiding ( Config )
import Evaluate
import Loader
import Network
import Trainer

import Prelude                                          as P
import Data.Functor
import Data.Label                                       ( get )
import System.Exit
import System.Environment
import System.Random

-- import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Examples.Internal          as A hiding ( bench )


main :: IO ()
main = do
  beginMonitoring

  (conf, opts, rest)       <- parseArgs options defaults header footer
  (fileImages, fileLabels)  <- case rest of
    (i:l:_) -> return (i,l)
    _       -> withArgs ["--help"] $ parseArgs options defaults header footer
            >> exitSuccess

  putStrLn "Loading images"
  images <- readImages fileImages
  putStrLn "Loading labels"
  labels <- readLabels fileLabels

  let backend = get optBackend opts
      eta     = get configEta conf
      mtestImages = get configTest conf
      mtestLabels = get configTestLabels conf

  putStrLn "Training network with supplied image set"
  (l1,l2) <- train3 backend eta images labels

  case (mtestImages, mtestLabels) of
    (Just fileTestImages, Just fileTestLabels) -> do
      putStrLn "Loading test images"
      testImages <- readImages fileTestImages
      putStrLn "Loading test labels"
      testLabels <- readLabels fileTestLabels
      putStrLn "Evaluating test set"
      let accuracy = evaluate backend [l1,l2] testImages testLabels
      putStrLn $ "The network has a " ++ show (accuracy * 100.0) ++ "% accuracy"
    _ -> return ()
