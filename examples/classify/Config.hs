{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config
  = Config
  {
    _configTest                 :: Maybe FilePath
  , _configTestLabels           :: Maybe FilePath
  , _configEta                  :: Float
  , _configEpochs               :: Int
  --
  , _configHelp                 :: Bool
  }

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  {
    _configTest                 = Nothing
  , _configTestLabels           = Nothing
  , _configEta                  = 3.0
  , _configEpochs               = 1
  , _configHelp                 = False
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option      ['t'] []
                (ReqArg (set configTest . Just) "FILE")
                "The idx file that contains the testing images"

  , Option      ['l'] []
                (ReqArg (set configTestLabels . Just) "FILE")
                "The idx file that contains the testing labels"

  , Option      ['e'] []
                (ReqArg (set configEta . read) "FLOAT")
                "The learning rate (eta)"

  , Option      ['n'] []
                (ReqArg (set configEpochs . read) "INT")
                "The number of epochs"

  , Option      ['h', '?'] ["help"]
                (NoArg (set configHelp True))
                "show this help message"
  ]

header :: [String]
header =
  [ "accelerate-classify (c) [2015] The Accelerate Team"
  , ""
  , "Usage: accelerate-classify [OPTIONS] train-images.idx3-ubyte train-labels.idx1-ubyte"
  , ""
  ]

footer :: [String]
footer = [ "" ]
