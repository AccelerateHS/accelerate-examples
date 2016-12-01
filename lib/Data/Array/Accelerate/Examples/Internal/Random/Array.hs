{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
-- Module:      : Data.Array.Accelerate.Examples.Internal.Random.Array
-- Copyright    : [2014] Trevor L. McDonell
-- License      : BSD3
--
-- Maintainer   : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability    : experimental
-- Portability  : non-portable (GHC extensions)
--

module Data.Array.Accelerate.Examples.Internal.Random.Array (

  -- * Generating random arrays
  (:~>),
  uniform, uniformR, standard, normal,
  randomArray, randomArrayWithSeed, randomArrayWithSystemRandom, randomArrayIO

) where

import System.Random.MWC                         hiding ( uniform, uniformR )
import qualified System.Random.MWC               as R
import qualified System.Random.MWC.Distributions as R

import Data.Array.Accelerate                     as A
import Data.Array.Accelerate.Array.Data          as A
import Data.Array.Accelerate.Array.Sugar         as Sugar


-- | A PNRG from indices to variates
--
type sh :~> e = sh -> GenIO -> IO e


-- | Uniformly distributed random variates.
--
uniform :: Variate e => sh :~> e
uniform _ = R.uniform

-- | Uniformly distributed random variates in a given range.
--
uniformR :: Variate e => (e, e) -> sh :~> e
uniformR bounds _ = R.uniformR bounds

-- | Normally distributed random variates with a mean of 0 and a standard
-- deviation of 1.
--
standard :: Shape sh => sh :~> Double
standard _ = R.standard

-- | Normally distributed random variates.
--
normal :: Shape sh => Double -> Double -> sh :~> Double
normal mean stddev _ = R.normal mean stddev

-- | Generate an array of random values using the supplied generator function.
--   The generator for variates is initialised with a fixed seed.
--
randomArray :: (Shape sh, Elt e) => sh :~> e -> sh -> Array sh e
randomArray f sh
  = let
        (adata, _)      = runArrayData $ do
                            gen <- create
                            arr <- runRandomArray f sh gen
                            return (arr, undefined)
    in
    adata `seq` Array (fromElt sh) adata

randomArrayIO :: (Shape sh, Elt e) => sh :~> e -> sh -> IO (Array sh e)
randomArrayIO f sh = do
   gen <- create
   arr <- runRandomArray f sh gen
   return (Array (fromElt sh) arr)


-- | Generate an array of random values using a supplied generator function and
--   seed value.
--
randomArrayWithSeed :: (Shape sh, Elt e) => Seed -> sh :~> e -> sh -> Array sh e
randomArrayWithSeed seed f sh
  = let
        (adata, _)      = runArrayData $ do
                            gen <- restore seed
                            arr <- runRandomArray f sh gen
                            return (arr, undefined)
    in
    adata `seq` Array (fromElt sh) adata


-- | Generate an array of random values using a supplied generator function,
--   initialised with the system's source of pseudo-random numbers.
--
--   TODO: find a way to do this directly, without going via save/restore.
--
randomArrayWithSystemRandom
    :: forall sh e. (Shape sh, Elt e)
    => sh :~> e
    -> sh
    -> IO (Array sh e)
randomArrayWithSystemRandom f sh
  = do
       seed   <- withSystemRandom (asGenIO save)
       return $! randomArrayWithSeed seed f sh


-- Common function to create a mutable array and fill it with random values
--
runRandomArray
    :: (Shape sh, Elt e)
    => sh :~> e
    -> sh
    -> GenIO
    -> IO (MutableArrayData (EltRepr e))
runRandomArray f sh gen
  = do
      arr <- newArrayData $! Sugar.size sh
      let write ix = unsafeWriteArrayData arr (Sugar.toIndex sh ix)
                   . fromElt =<< f ix gen

      iter sh write (>>) (return ())
      return arr
