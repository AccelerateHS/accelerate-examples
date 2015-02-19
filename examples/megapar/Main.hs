{-# LANGUAGE CPP #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-cse #-}

-- This is a test for concurrent kernel execution. However, there isn't a good
-- way to test whether it works other than loading the program into nvvp \=
--
--module Test4 where
module Main where 
import Prelude                                  as P

import Data.Array.Accelerate                    as A

import System.Environment

import Config 
import Data.Array.Accelerate.Examples.Internal as A 

import Data.Label
-- runIt :: Arrays a => Acc a -> a 
-- runIt = C.run

main :: IO ()
main
  = do  beginMonitoring
        argv                    <- getArgs
        (conf, opts, rest)      <- parseArgs options defaults header footer argv

--        setFlags [dump_gc, dump_cc, verbose] 

        let n           = get configN conf
            backend     = get optBackend opts

            -- -- Advancing the simulation
            -- --
            -- advance     = advanceWorld step
            -- step        = P.curry
            --             $ run1 backend
            --             $ A.uncurry
            --             $ advanceBodies (solver $ constant epsilon)


        -- Forward unto dawn
        --

        runBenchmarks opts rest
          [ bench "megapar" $ whnf (run backend . megapar) n ]


  
loop :: Exp Int -> Exp Int
loop ticks = A.while (\i -> i <* clockRate * ticks) (+1) 0
  where
    clockRate   = 900000

{-# NOINLINE megapar #-}
megapar :: Int -> Acc ( Vector Int
                      , Vector Int
                      , Vector Int
                      , Vector Int
                      , Vector Int
                      , Vector Int
                      , Vector Int
                      , Vector Int)
megapar d = 
  lift ( compute $ A.map loop (use $ fromList (Z:.1) [d])
       , compute $ A.map loop (use $ fromList (Z:.1) [d])
       , compute $ A.map loop (use $ fromList (Z:.1) [d])
       , compute $ A.map loop (use $ fromList (Z:.1) [d])
       , compute $ A.map loop (use $ fromList (Z:.1) [d])
       , compute $ A.map loop (use $ fromList (Z:.1) [d])
       , compute $ A.map loop (use $ fromList (Z:.1) [d])
       , compute $ A.map loop (use $ fromList (Z:.1) [d]))


