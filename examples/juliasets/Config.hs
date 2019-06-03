{-# LANGUAGE TemplateHaskell #-}

module Config where

import           Data.Label
import           System.Console.GetOpt


data Config = Config
  {
    _configWidth  :: Int
  , _configHeight :: Int
  }
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  { _configWidth        = 800
  , _configHeight       = 800
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option []   ["width"]       (ReqArg (set configWidth  . read) "INT")         "visualisation width  (800)"
  , Option []   ["height"]      (ReqArg (set configHeight . read) "INT")         "visualisation height (800)"
  ]

header :: [String]
header =
  [ "accelerate-juliasets (c) [2011..2016] Roger Bosman"
  , ""
  , "Usage: accelerate-mandelbrot [OPTIONS]"
  , "Or, if using stack: stack exec accelerate-juliasets [OPTIONS]"
  , ""
  ]

footer :: [String]
footer =
  [ "Controls:"
  , "Key          Action"
  , "ESC          Exit simulation"
  , "W            Zoom in"
  , "S            Zoom out"
  , "A            Speed up simulation by 25%"
  , "D            Slow down simulation by 25%"
  , "Space        Pause/unpause simulation"
  , "Arrow up     Pan view up"
  , "Arrow down   Pan view down"
  , "Arrow left   Pan view left"
  , "Arrow right  Pan view right"
  , ""
  ]
