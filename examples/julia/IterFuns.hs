{-# LANGUAGE RecordWildCards #-}

module IterFuns where

import           Data.Array.Accelerate              as A
import           Data.Array.Accelerate.Data.Complex as A

import           Lib
import           World

iterQuad :: Float
         -> Exp Float              -- time
         -> (Exp Float, Exp Float) -- unpacked complex`
         -> Exp IComplex
iterQuad c time (zx, zy) =
  let c' = mkPolar (lift c) time
      cx = real c'
      cy = imag c'

      zx' = zx * zx - zy * zy + cx :: Exp Float
      zy' = 2 * zx * zy  + cy :: Exp Float
   in lift $ zx' :+ zy'
