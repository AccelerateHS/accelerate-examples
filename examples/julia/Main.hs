{-# LANGUAGE RecordWildCards #-}
-- A Julia set animation
-- Originally written by Roger Bosman
--

import World
import Config

import Data.Label

import Prelude                                                      as P
import Data.Array.Accelerate.Examples.Internal                      as A
import qualified Graphics.Gloss.Interface.IO.Game                   as G

-- Main ------------------------------------------------------------------------

main :: IO ()
main = do
  beginMonitoring
  (conf, opts, rest) <- parseArgs options defaults header footer

  let world   = initialWorld conf opts
      width   = get configWidth conf
      height  = get configHeight conf

  runBenchmarks opts rest
    [ bench "julia" $ whnf renderWorld world ]

  runInteractive opts rest $
    G.playIO (G.InWindow "Julia" (width,height) (10,10))
             G.black
             60
             world
             draw
             (react conf opts)
             advance

