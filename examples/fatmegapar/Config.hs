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
  , _configHowFat       :: Int 
  }
  deriving Show

$(mkLabels [''Config])


defaults :: Config
defaults = Config
  {
    _configN            = 1
  , _configHowFat       = 2^20 -- A Million
  }


-- | The set of available command-line options
--
options :: [OptDescr (Config -> Config)]
options = 
  [ Option  ['n'] ["sequential depth multiplier"]
            (ReqArg (set configN . read) "INT")
            (describe configN "Higher number -> more expensive computation")
  , Option  ['m'] ["Parallel width of computation"]
            (ReqArg (set configHowFat . read) "INT")
            (describe configN "Higher number -> wider parallelism") 
  ]
  where
    describe f msg
      = msg ++ " (" ++ show (get f defaults) ++ ")"


-- | Process the command line options
--

header :: [String]
header =
  [ "fatmegapar - A hack "
  , ""
  , "Usage: fatmegapar [OPTIONS]"
  , ""
  ]

footer :: [String]
footer = [ "" ]

