{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE TypeApplications #-}

module Digest (readDict, extract)
  where

import MD5
import Config

import Control.Applicative
import Control.Monad
import Data.ByteString.Internal                                     ( w2c )
import Data.Label
import Data.Word
import Prelude
import qualified Data.ByteString                                    as S
import qualified Data.ByteString.Lazy.Char8                         as L
import qualified Data.Serialize                                     as S

import Data.Array.Accelerate.Array.Data                             as A
import Data.Array.Accelerate.Sugar.Array
import Data.Array.Accelerate.Sugar.Elt
import Data.Array.Accelerate.Sugar.Shape
import qualified Data.Array.Accelerate                              as A
import qualified Data.Array.Accelerate.Representation.Array         as R


-- MD5 block sizes
--
blockSizeBits, blockSizeBytes, blockSizeWords :: Int
blockSizeBits  = 512
blockSizeBytes = blockSizeBits `div` 8
blockSizeWords = blockSizeBytes `div` 4


-- Create an MD5 block from the given message. This appends The '1' bit to the
-- message, pads the block with zeros until the length in bits is 448, and
-- finally appends the length in bits (mod 2^64).
--
md5block :: L.ByteString -> S.ByteString
md5block msg = do
  let
      len               = fromIntegral (L.length msg)
      lenBits           = 8 * fromIntegral len
      lenZeroPad
        | len + 1 <= blockSizeBytes - 8 = (blockSizeBytes - 8) - (len + 1)
        | otherwise                     = (2 * blockSizeBytes - 8) - (len + 1)
  --
  S.runPut $! do
    S.putLazyByteString msg
    S.putWord8 0x80
    mapM_ S.putWord8 (replicate lenZeroPad 0)
    S.putWord64le lenBits


-- Create a dictionary of blocks ready to digest from the given bytestring. This
-- reads one entry per line. Because we only do a single MD5 chunk, we discard
-- any entries with (length > blockSizeBytes - 8 = 55) characters. Because of
-- this restriction, we also need to a precise count of how many entries will be
-- in the final array, since it is stored column-major and hence there is no
-- easy way to truncate it.
--
readDict :: Config -> FilePath -> IO Dictionary
readDict c fp = do
  entries <- length       . chunk <$> L.readFile fp
  blocks  <- map md5block . chunk <$> L.readFile fp

  let sh        = Z :. blockSizeWords :. entries
      (adata,_) = runArrayData $ do
        arr <- newArrayData (eltR @Word32) (size sh)

        let go !_ []     = return ()
            go !n (b:bs) = do
              foldM_ (\i w -> do writeArrayData (eltR @Word32) arr (toIndex sh (Z:.i:.n)) (fromElt w)
                                 return (i+1)) 0 (bytes b)
              go (n+1) bs

        go 0 blocks
        return (arr, 0 :: Word32)

  adata `seq` return $ Array (R.Array (fromElt sh) adata)

  where
    chunk = maybe id take (get configMaxWords c)
          . drop (get configSkipWords c)
          . filter (\w -> fromIntegral (L.length w) < blockSizeBytes - 8)
          . L.lines

    bytes = either error id
          . S.runGet (replicateM blockSizeWords S.getWord32le)

-- Extract a word from the dictionary at a given index
--
extract :: Dictionary -> Int -> L.ByteString
extract dict i
  | i > n       = error "extract: index too large"
  | otherwise   = L.takeWhile (/= w2c 0x80) bytes
  where
    Z :. _ :. n = A.arrayShape dict
    bytes       = S.runPutLazy $
      forM_ [0 .. blockSizeWords-1] $ \c -> S.putWord32le (dict `A.indexArray` (Z:.c:.i))

