{-# LANGUAGE CPP #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where 
import Prelude                                  as P
import Data.Array.Accelerate                    as A
import System.Environment
import Config 
import Data.Array.Accelerate.Examples.Internal  as A 
import Data.Label

main :: IO ()
main
  = do  beginMonitoring
        argv                    <- getArgs
        (conf, opts, rest)      <- parseArgs options defaults header footer argv


        let n           = get configN conf
            backend     = get optBackend opts
            xs          = A.use $ fromList (Z :. n) [0..]
            ys          = A.use $ fromList (Z :. n) [100..]

        runBenchmarks opts rest
          [ bench "dotp" $ whnf (run backend . dotp') (xs, ys) ]

dotp' :: (Acc (Array DIM1 Int), Acc (Array DIM1 Int)) -> Acc (Array DIM0 Int)
dotp' (xs, ys) = dotp xs ys

{-# NOINLINE dotp #-}
dotp :: Acc (Array DIM1 Int) -> Acc (Array DIM1 Int) -> Acc (Array DIM0 Int)
dotp xs ys = A.fold (+) 0 $ A.zipWith (*) xs ys

-- just a lonely dot product
