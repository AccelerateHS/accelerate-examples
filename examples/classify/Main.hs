{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Config                                           hiding ( Config )
import Network

import Prelude                                          as P
import Control.Exception                                ( evaluate )
import Data.Functor
import Data.Label                                       ( get )
import System.Environment
import System.Random
import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Examples.Internal          as A hiding ( bench )


main :: IO ()
main = do
  beginMonitoring

  (conf, opts, rest)       <- parseArgs options defaults header footer

  return ()
