{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config
  = Config
  {

  }

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  {
  }

options :: [OptDescr (Config -> Config)]
options =
  [ ]


header :: [String]
header =
  [ "accelerate-classify (c) [2015] The Accelerate Team"
  , ""
  , "Usage: accelerate-classify [OPTIONS]"
  , ""
  ]

footer :: [String]
footer = [ "" ]
