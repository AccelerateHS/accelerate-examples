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
            m           = get configHowFat conf 
            backend     = get optBackend opts
        
        -- Forward unto dawn
        --

        runBenchmarks opts rest
          [ bench "megapar" $ whnf (run backend . (megapar m)) n ]



loop :: Int -> Exp DIM1 -> Exp Int
loop n ix  = A.while (\i -> i <* clockRate * (constant n) ) (+1) ix'
  where
    ix' = unindex1 ix 
    clockRate   = 90000


{-# NOINLINE megapar #-}
megapar :: Int -> Int -> Acc ( Vector Int
                             , Vector Int
                             , Vector Int
                             , Vector Int)
megapar m n = 
  lift ( compute $ A.generate (constant (Z :. m)) (loop n)  
       , compute $ A.generate (constant (Z :. m)) (loop n)
       , compute $ A.generate (constant (Z :. m)) (loop n)
       , compute $ A.generate (constant (Z :. m)) (loop n) )


