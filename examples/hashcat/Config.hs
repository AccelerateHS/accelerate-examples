{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import Data.Label
import System.Console.GetOpt


data Config
  = Config
  {
    -- Input data
    _configStrings              :: [String]
  , _configDict                 :: FilePath
  , _configMaxWords             :: Maybe Int
  , _configSkipWords            :: Int
  , _configSeq                  :: Bool
  }
  deriving Show

$(mkLabels [''Config])

defaults :: Config
defaults = Config
  {
    _configStrings              = []
  , _configDict                 = []
  , _configMaxWords             = Nothing
  , _configSkipWords            = 0
  , _configSeq                  = False
  }


-- | The set of available command-line options
--
options :: [OptDescr (Config -> Config)]
options =
  [ Option      ['s'] []
                (ReqArg (modify configStrings . (:)) "STRING")
                "Lookup the plain text of a given checksum"

  , Option      ['d'] ["dictionary"]
                (ReqArg (set configDict) "FILE")
                "Plain text word list to search against"

  , Option      ['j'] ["skip-words"]
                (ReqArg (set configSkipWords . read) "INT")
                "Skip this many entries from the start of the word list"

  , Option      ['n'] ["max-words"]
                (ReqArg (set configMaxWords . Just . read) "INT")
                "Use at most this many words from the list"

  , Option      [] ["sequences"]
                (NoArg (set configSeq True))
                "Use Sequences"
  ]


-- | Process the command line options
--
header :: [String]
header =
  [ "accelerate-hashcat (c) [2013] The Accelerate Team"
  , ""
  , "Usage: accelerate-hashcat -d dictionary [OPTIONS] [file ...]"
  , ""
  ]

footer :: [String]
footer = [ "" ]

