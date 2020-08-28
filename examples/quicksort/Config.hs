{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config
  = Config
  { _configRandomSize :: Maybe Int
  }

$(mkLabels [''Config])

defaults :: Config
defaults = Config Nothing

options :: [OptDescr (Config -> Config)]
options =
  [ Option [] ["size"] (ReqArg (set configRandomSize . Just . read) "INT") "number of elements for a random input"
  ]


header :: [String]
header =
  [ "accelerate-quicksort (c) [2019] The Accelerate Team"
  , ""
  , "Usage: accelerate-quicksort input.txt"
  , "  where input.txt is a file containing a list of integers seperated by spaces"
  , ""
  , "Usage: accelerate-quicksort --size n"
  , "  where n is a number, denoting the size of a random input"
  , ""
  ]

footer :: [String]
footer = [ "" ]

