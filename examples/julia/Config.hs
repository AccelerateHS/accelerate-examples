{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config = Config
  {
    _configWidth  :: Int
  , _configHeight :: Int
  }
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  { _configWidth    = 800
  , _configHeight   = 600
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option []   ["width"]       (ReqArg (set configWidth  . read) "INT")         "visualisation width  (800)"
  , Option []   ["height"]      (ReqArg (set configHeight . read) "INT")         "visualisation height (600)"
  ]

header :: [String]
header =
  [ "accelerate-julia (c) [2019] The Accelerate Team"
  , ""
  , "Usage: accelerate-julia [OPTIONS]"
  , ""
  ]

footer :: [String]
footer =
  [ ""
  , "Runtime usage:"
  , "     ESC           quit"
  , "     mouse drag    translate display"
  , "     w/s           zoom in/out"
  , "     a/d           iteration count"
  , "     z/c           escape radius"
  , "     q/e           speed up/slow down"
  , "     left/right    forward/backwards in time"
  , "     0 .. 9        select presets"
  , "     r             reset"
  , "     .             print current configuration"
  , "     space         pause/resume"
  , ""
  ]

