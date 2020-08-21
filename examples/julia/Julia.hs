{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Julia where

import Data.Array.Accelerate                              hiding ( fromInteger )
import Data.Array.Accelerate.Data.Complex

import Prelude                                            ( fromInteger )
import qualified Prelude                                  as P


julia
    :: forall a. (Num a, RealFloat a, FromIntegral Int a)
    => Int                                                -- ^ image width
    -> Int                                                -- ^ image height
    -> (Exp Float -> Exp (Complex a) -> Exp (Complex a))  -- ^ iteration function
    -> Acc (Scalar Float)                                 -- ^ current time
    -> Acc (Scalar a)                                     -- ^ centre x
    -> Acc (Scalar a)                                     -- ^ centre y
    -> Acc (Scalar a)                                     -- ^ view width
    -> Acc (Scalar Int32)                                 -- ^ iteration limit
    -> Acc (Scalar a)                                     -- ^ divergence radius
    -> Acc (Array DIM2 (Complex a, Int32))
julia screenX screenY next (the -> t) (the -> x0) (the -> y0) (the -> width) (the -> limit) (the -> radius) =
  generate
    (constant (Z :. screenY :. screenX))
    (\ix -> let z0 = complexOfPixel ix
                zn = while (\zi -> snd zi       < limit
                                && dot (fst zi) < radius)
                           step
                           (T2 z0 0)
             in zn)

  where
    complexOfPixel :: Exp DIM2 -> Exp (Complex a)
    complexOfPixel (unlift -> Z :. y :. x) =
      let
          height = P.fromIntegral screenY / P.fromIntegral screenX * width
          xmin   = x0 - width  / 2
          ymin   = y0 - height / 2
          --
          re     = xmin + (fromIntegral x * width)  / fromIntegral (constant screenX)
          im     = ymin + (fromIntegral y * height) / fromIntegral (constant screenY)
      in
      lift (re :+ im)

    -- Divergence condition
    --
    dot :: Exp (Complex a) -> Exp a
    dot (unlift -> x :+ y) = x*x + y*y

    -- Take a single step of the recurrence relation
    --
    step :: Exp (Complex a, Int32) -> Exp (Complex a, Int32)
    step (T2 z i) = T2 (next t z) (i + 1)

