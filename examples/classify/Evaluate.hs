{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ViewPatterns     #-}
module Evaluate where

import Data.Array.Accelerate                                 hiding (tail, init )
import Data.Array.Accelerate.Examples.Internal
import Prelude                                               hiding ( fst, snd, fromIntegral, map, (==), (>), Num(..), Ord(..) )
import qualified Prelude                                     as P

import Image
import Network

evaluate :: Backend -> [Layer] -> Array DIM3 Word8 -> Vector Word8 -> Float
evaluate backend n images labels =
  let inputs   = allImages images
      outputs  = mapSeq (unit . unindex1 . argMax . feedforward (P.map lift n)) inputs
      expected = mapSeq (reshape index0) $ subarrays (index1 1) labels
      correct  = collect . sumSeq
               $ zipWithSeqE (\o e -> fromIntegral o == e ? ((1 :: Exp Int),0)) outputs expected
      ratio    = fromIntegral (the correct) / fromIntegral (lift c)
      Z :. c :. _ :. _ = arrayShape images
  in indexArray (run backend (unit ratio)) Z

sumSeq :: (Elt e, Num e) => Seq [Scalar e] -> Seq (Scalar e)
sumSeq = foldSeqE (+) 0

argMax :: (Elt e, Num e, Ord e, Shape sh) => Acc (Array sh e) -> Exp sh
argMax = fst . the . fold1All f . indexed
  where f a b = snd a > snd b ? (a,b)
