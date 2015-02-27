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
            arr         = A.use $ fromList (Z :. n :. n) [0.0..] :: Acc (Array DIM2 Float)
            brr         = A.use $ fromList (Z :. n :. n) [100.0..] :: Acc (Array DIM2 Float)            

        runBenchmarks opts rest
          [ bench "mmult" $ whnf (run backend . mmult') (arr, brr) ]


mmult' :: forall e.
          (Elt e, IsNum e) =>
          (Acc (Matrix e), Acc (Matrix e)) -> Acc (Matrix e)
mmult' (arr,brr) = matMul arr brr

type Matrix a = Array DIM2 a

matMul :: (IsNum e, Elt e) => Acc (Matrix e) -> Acc (Matrix e) -> Acc
          (Matrix e)
matMul arr brr
  = A.fold (+) 0
   $ A.zipWith (*) arrRepl brrRepl
   where
     Z :. rowsA :. _     = unlift (shape arr)    :: Z :. Exp Int :. Exp Int
     Z :. _     :. colsB = unlift (shape brr)    :: Z :. Exp Int :. Exp Int

     arrRepl             = A.replicate (lift $ Z :. All   :. colsB :. All) arr
     brrRepl             = A.replicate (lift $ Z :. rowsA :. All   :. All) (A.transpose brr)
