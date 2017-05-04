{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns     #-}

module MD5 (

  Dictionary, MD5,
  hashcatWord, hashcatDict, hashcatSeq, readMD5, showMD5, md5Round

) where


import Numeric
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List                                ( foldl' )
import Data.ByteString.Lex.Integral             ( readHexadecimal )
import qualified Data.Serialize                 as S
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import Prelude                                  as P hiding ( Eq(..), (&&) )

import Data.Array.Accelerate                    hiding ( Ord(..) )
import Data.Array.Accelerate.Data.Bits          as A
import qualified Data.Array.Accelerate          as A


hashcatSeq :: Bool -> Dictionary -> Acc (Scalar MD5.MD5) -> Acc (Scalar Int)
hashcatSeq colMajor dict digest
  = unit . A.fst . the . collect
  $ foldSeqFlatten find (unit (lift (-1 :: Int, 0 :: Int))) (words (use dict))
  where
    words :: Acc Dictionary -> Seq [Vector Word32]
    words = if colMajor then toSeqInner else toSeqOuter
    find fi ixs vs =
      let
          (found, i) = unlift (A.the fi)
          dict'      = reshape (A.lift (Z :. size ixs :. constant 16)) vs
          found'     = hashcatDict False dict' digest
      in
      unit $ lift (the found' A.> -1 ? (i + the found', found), i + (size ixs))


-- Generate an MD5 hash for every word in the dictionary, and if an entry
-- matches the given unknown md5, returns the index into the dictionary of said
-- match. If not found, this returns (-1).
--
hashcatDict :: Bool                 -- ^ column major order?
            -> Acc Dictionary
            -> Acc (Scalar MD5)
            -> Acc (Scalar Int)
hashcatDict colMajor dict passwd
  = reshape (constant Z)
  $ permute const res (\ix -> crypt A.! ix A.== the passwd ? (constant (Z:.0), ignore))
                      (enumFromN (index1 n) 0)
  where
    n       = A.size crypt
    res     = use $ fromList (Z:.1) [-1]    :: Acc (Vector Int)
    crypt   = md5 colMajor dict


-- Generate an MD5 hash for a single word, and if an entry matches the
-- given unknown md5, returns the given index. If not matched, this
-- returns (-1).
--
hashcatWord :: Acc (Scalar MD5)
            -> Acc (Vector Word32)
            -> Acc (Scalar Int)
            -> Acc (Scalar Int)
hashcatWord passwd word ix
  = let crypt = md5Round (\i -> word A.! index1 i)
    in  unit (crypt A.== the passwd ? (the ix, -1))


-- An MD5 round processes 512 bits of the input, as 16 x 32-bit values. We
-- require that the input dictionary of words to hash is of dimension
-- (Z :. 16 :. n), which will then output n MD5 hashes values.
--
-- NOTE: All values are unsigned 32-bit integers and wrap modulo 2^32 when
--       calculating. All values are little endian.
--
-- TODO: How to apply salt to passwords?
--
type MD5        = (Word32, Word32, Word32, Word32)
type Dictionary = Array DIM2 Word32

md5 :: Bool -> Acc Dictionary -> Acc (Vector MD5)
md5 columnMajor dict
  = let n = if columnMajor
              then A.snd . unindex2 $ A.shape dict
              else A.fst . unindex2 $ A.shape dict
    in
    A.generate (index1 n)
               (\(unindex1 -> ix) -> md5Round (\i -> if columnMajor
                                                       then dict A.! index2 i ix
                                                       else dict A.! index2 ix i))

md5Round :: (Exp Int -> Exp Word32) -> Exp MD5
md5Round fetch
  = lift
  $ P.foldl step (a0,b0,c0,d0) [0..64]
  where
    step (a,b,c,d) i
      | i < 16    = shfl ((b .&. c) .|. ((complement b) .&. d))
      | i < 32    = shfl ((b .&. d) .|. (c .&. (complement d)))
      | i < 48    = shfl ((b `xor` c `xor` d))
      | i < 64    = shfl (c `xor` (b .|. (complement d)))
      | otherwise = (a+a0,b+b0,c+c0,d+d0)
      where
        shfl f = (d, b + ((a + f + k i + get i) `rotateL` r i), b, c)

    get :: Int -> Exp Word32
    get i
      | i < 16    = fetch (constant i)
      | i < 32    = fetch (constant ((5*i + 1) `rem` 16))
      | i < 48    = fetch (constant ((3*i + 5) `rem` 16))
      | otherwise = fetch (constant ((7*i)     `rem` 16))

    -- Initial values. For a multi-round implementation we would initialise the
    -- context with these values, and then after applying the round update the
    -- context with the addition of the new result. Since we only ever apply one
    -- round, we begin with these values and add them again at the end, which
    -- seems odd but is correct.
    --
    a0, b0, c0, d0 :: Exp Word32
    a0 = 0x67452301
    b0 = 0xefcdab89
    c0 = 0x98badcfe
    d0 = 0x10325476

    -- The binary integer part of sines and cosines (in radians) as constants
    --
    --   K[i] := floor(abs(sin(i + 1)) × (2 pow 32))
    --
    k :: Int -> Exp Word32
    k i = constant (ks P.!! i)
      where
        ks = [ 0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee
             , 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501
             , 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be
             , 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821
             , 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa
             , 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8
             , 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed
             , 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a
             , 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c
             , 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70
             , 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05
             , 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665
             , 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039
             , 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1
             , 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1
             , 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391 ]

    -- Specifies the per-round shift amounts
    --
    r :: Int -> Exp Int
    r i = constant (rs P.!! i)
      where
        rs = [ 7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22
             , 5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20
             , 4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23
             , 6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21 ]

-- Display the result in big endian hexadecimal format, which is consistent with
-- other MD5 hash implementations.
--
showMD5 :: MD5 -> String
showMD5 (a,b,c,d) =
  let bs                = S.runPutLazy $ S.putWord32be d >> S.putWord32be c >> S.putWord32be b >> S.putWord32be a
      showsHex str w    = let x = showHex w str
                          in if P.length x < P.length str + 2
                                then '0':x
                                else x
  in
  foldl' showsHex "" (L.unpack bs)


-- Read an input hexadecimal string into our MD5 representation. This string is
-- the same as that displayed by the standard md5 utility, and also produced by
-- 'showMD5'.
--
readMD5 :: L.ByteString -> MD5
readMD5 =
  let get = (,,,) <$> readHex32be <*> readHex32be <*> readHex32be <*> readHex32be

      readHex32be = do
        s <- B.concat . P.reverse <$> replicateM 4 (S.getBytes 2)
        return . P.fst $ fromMaybe (error "readHex32be: parse failure") (readHexadecimal s)
  in
  either error id . S.runGetLazy get

