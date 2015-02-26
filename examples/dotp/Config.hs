{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Char
import Data.List
import Data.Label
import System.Console.GetOpt


data Config
  = Config
  {
    -- System setup
    _configN            :: Int
  }
  deriving Show

$(mkLabels [''Config])


defaults :: Config
defaults = Config
  {
    _configN            = 1
  }


-- | The set of available command-line options
--
options :: [OptDescr (Config -> Config)]
options =
  [ Option  ['n'] ["a number"]
            (ReqArg (set configN . read) "INT")
            (describe configN "Higher number -> more expensive computation")
  ]
  where
    describe f msg
      = msg ++ " (" ++ show (get f defaults) ++ ")"


-- | Process the command line options
--

header :: [String]
header =
  [ "dotp "
  , ""
  , "Usage: accelerate-blackscholes [OPTIONS]"
  , ""
  ]

footer :: [String]
footer = [ "" ]

