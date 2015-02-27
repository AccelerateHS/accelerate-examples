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

        let n           = get configN conf
            backend     = get optBackend opts

            a,b,c,d :: Vector Int 
            a      = randomArray uniform (Z :. n)
            b      = randomArray uniform (Z :. n)
            c      = randomArray uniform (Z :. n)
            d      = randomArray uniform (Z :. n)
            
        
        -- Forward unto dawn
        --

        runBenchmarks opts rest
          [ bench "memboundmegapar" $ whnf (run backend . megapar) (A.use (a,b,c,d)) ]

{-# NOINLINE megapar #-}
megapar :: Acc ( Vector Int
               , Vector Int
               , Vector Int
               , Vector Int)

        -> Acc ( Vector (Int,Int)
               , Vector (Int,Int)
               , Vector (Int,Int)
               , Vector (Int,Int))
megapar arrs =
  let (a,b,c,d) = A.unlift arrs
  in A.lift ( compute $ A.map f a
            , compute $ A.map f b
            , compute $ A.map f c
            , compute $ A.map f d)
  where
    f x = A.lift (2*x,1+x) 


