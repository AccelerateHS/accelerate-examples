{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Prelude.Sequences (

  test_sequences

) where

import Prelude                                                  as P
import Data.Label
import Data.Maybe
import Data.Typeable
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck                                          hiding ( generate, collect )

import Config
import QuickCheck.Arbitrary.Array                               ()
import Data.Array.Accelerate.Examples.Internal
import Data.Array.Accelerate                                    as A
import Data.Array.Accelerate.Array.Sugar                        as Sugar

iota :: Int -> Acc (Vector Int)
iota n = generate (index1 (constant n)) unindex1

iota' :: Acc (Scalar Int) -> Acc (Vector Int)
iota' n = generate (index1 (the n)) unindex1

-- We need to introduce a normal form for arrays where any arrays of size zero
-- are coerced into an array with the empty shape. e.g. An array of shape
-- (Z:.2:.0) becomes and array of shape (Z:.0:.0)
--
normalise :: forall sh e. Shape sh => Array sh e -> Array sh e
normalise (Array sh adata) = Array (if Sugar.size (toElt sh :: sh) P.== 0 then fromElt (Sugar.empty :: sh) else sh) adata

-- iotaChunk :: Int -> Int -> Acc (Array (Z :. Int :. Int) Int)
-- iotaChunk n b = reshape (constant (Z :. b :. n)) $ generate (index1 (constant (n * b))) unindex1

idSequence
    :: forall sh a. (Shape sh, Slice sh, Elt a)
    => Acc (Array (sh :. Int) a)
    -> Acc (Array (sh :. Int) a)
idSequence xs
  = collect . tabulate
  $ toSeqOuter xs

idSequenceRef :: (Shape sh, Elt a) => (Array (sh :. Int) a) -> (Array (sh :. Int) a)
idSequenceRef = id

sumMaxSequence :: (A.Num a, A.Ord a, A.Bounded a) => Acc (Vector a) -> Acc (Scalar a, Scalar a)
sumMaxSequence xs = collect $
  let xs' = toSeqInner xs
  in lift ( foldSeqE (+) 0 xs'
          , foldSeqE A.max minBound xs')

sumMaxSequenceRef :: (Elt a, P.Ord a, P.Bounded a, P.Num a) => Vector a -> (Scalar a, Scalar a)
sumMaxSequenceRef xs = ( fromList Z . (:[]) . P.sum                    . toList $ xs
                       , fromList Z . (:[]) . P.foldl (P.max) minBound . toList $ xs
                       )

scatterSequence :: A.Num a => Acc (Vector a, Vector (Int, a)) -> Acc (Vector a)
scatterSequence input = collect
  $ foldSeqFlatten f (afst input)
  $ toSeqInner (asnd input)
  where
    f xs' _ upd =
      let (to, ys) = A.unzip upd
      in permute (+) xs' (index1 . (`mod` A.size (afst input)) . (to A.!)) ys

scatterSequenceRef :: (Elt a, P.Num a) => (Vector a, Vector (Int, a)) -> Vector a
scatterSequenceRef (vec, vec_upd) =
  let xs                = toList vec
      updates           = toList vec_upd
      n                 = P.length xs
      ys                = P.foldl f xs updates
      f xs' (i, x)      = [ if j P.== i `P.mod` n then x P.+ y else y | (j, y) <- P.zip [0..] xs']
  in
  fromList (Z :. n) ys

logsum :: (A.Floating a, A.FromIntegral Int a) => Int -> Acc (Scalar a)
logsum n = collect
  $ foldSeqE (+) 0.0
  $ mapSeq (A.map (log . A.fromIntegral . (+1)))
  $ toSeqInner (iota n)

logsumRef :: (Elt a, P.Floating a) => Int -> Scalar a
logsumRef n = fromList Z [P.sum [log (P.fromIntegral i) | i <- [1..n]]]

-- nestedSequence :: Int -> Int -> Acc (Vector Int)
-- nestedSequence n m = asnd . collect
--   $ fromSeq
--   $ mapSeq
--   (\ i -> collect
--           $ foldSeq (+) 0
--           $ mapSeq (A.zipWith (+) i)
--           $ toSeqInner (iota m)
--   )
--   $ toSeqInner (iota n)

-- nestedSequenceRef :: Int -> Int -> Vector Int
-- nestedSequenceRef n m = fromList (Z :. n) [P.sum [i + j | j <- [0..m-1]] | i <- [0..n-1]]
--
-- nestedIrregularSequence :: Int -> Acc (Vector Int)
-- nestedIrregularSequence n = asnd . collect
--   $ fromSeq
--   $ mapSeq
--   (\ i -> collect
--         $ foldSeq (+) 0
--         $ mapSeq (A.zipWith (+) i)
--         $ toSeqInner (iota' i)
--   )
--   $ toSeqInner (iota n)

-- nestedIrregularSequenceRef :: Int -> Vector Int
-- nestedIrregularSequenceRef n = fromList (Z :. n) [P.sum [i + j | j <- [0..i-1]] | i <- [0..n-1]]
--
-- deepNestedSequence :: Int -> Acc (Vector Int)
-- deepNestedSequence n = asnd . collect
--   $ fromSeq
--   $ mapSeq
--   (\ i -> asnd . collect
--         $ fromSeq
--         $ mapSeq
--         (\ j -> collect
--               $ foldSeqE (+) 0
--               $ mapSeq
--               (\ k -> collect
--                     $ foldSeqE (+) 0
--                     $ toSeqInner (iota' k)
--               )
--               $ toSeqInner (iota' j)
--         )
--         $ toSeqInner (iota' i)
--   )
--   $ toSeqInner (iota n)

-- deepNestedSequenceRef :: Int -> Vector Int
-- deepNestedSequenceRef n = fromList (Z :. P.length xs) xs
--   where xs = [P.sum [x | k <- [0..j-1], x <- [0..k-1]] | i <- [0..n-1], j <- [0..i-1]]

irregular :: Int -> Acc (Scalar Int)
irregular n = collect
  $ foldSeqE (+) 0
  $ let s = toSeqInner (iota n)
    in zipWithSeq (\ x y -> A.zipWith (-) (A.sum x) (A.product y))
         (mapSeq iota' s)
         (mapSeq (iota' . A.map (constant n -)) s)

enumeration :: Int -> Int -> Acc (Vector DIM1, Vector Int)
enumeration n x = collect . fromSeq
                $ produce (lift n) (\i -> A.map (* the i) (iota x))

enumerationIrregular :: Int -> Acc (Vector DIM1, Vector Int)
enumerationIrregular n = collect . fromSeq
                       $ produce (lift n) (\i -> iota' i)

regularFold :: Shape sh => Acc (Array (sh:.Int:.Int) Int) -> Acc (Array (sh:.Int) Int)
regularFold = collect . tabulate . mapSeq (fold (+) 0) . toSeqInner

irregularFold :: Int -> Acc (Array DIM1 Int)
irregularFold n = collect . tabulate . mapSeq (fold (+) 0) $ produce (lift n) iota'

append :: Acc (Array (Z :. Int :. Int) Int, Array (Z :. Int :. Int) Int) -> Acc (Vector Int)
append input = asnd $ collect
  $ fromSeq
  $ zipWithSeq (A.++)
      (toSeqOuter (afst input))
      (toSeqOuter (asnd input))

appendIrregular :: Int -> Int -> Acc (Vector Int)
appendIrregular x y = asnd $ collect
  $ fromSeq
  $ zipWithSeq (A.++)
      (produce (lift x) iota')
      (produce (lift y) iota')

enumerationRef :: Int -> Int -> (Vector DIM1, Vector Int)
enumerationRef n x =
  let
    shs = P.replicate n (Z:.x)
    xs = [ P.map (*i) [0..x-1] | i <- [0..n-1]]
    res = concat xs
  in ( fromList (Z :. P.length shs) shs
     , fromList (Z :. P.length res) res)

enumerationIrregularRef :: Int -> (Vector DIM1, Vector Int)
enumerationIrregularRef n =
  let
    shs = [ Z:.i | i <- [0..n-1]]
    xs = [ [0..i-1] | i <- [0..n-1]]
    res = concat xs
  in ( fromList (Z :. P.length shs) shs
     , fromList (Z :. P.length res) res)

appendRef :: (Array (Z :. Int :. Int) Int, Array (Z :. Int :. Int) Int) -> Vector Int
appendRef (a, b) =
  let
    (Z:. n :. m) = Sugar.shape a
    (Z:. r :. c) = Sugar.shape b
    xs = [ [ a Sugar.! (Z :. i :. j) | j <- [0..m-1]] | i <- [0..n-1]]
    ys = [ [ b Sugar.! (Z :. i :. j) | j <- [0..c-1]] | i <- [0..r-1]]
    zs = P.zipWith (P.++) xs ys
    res = concat zs
  in fromList (Z :. P.length res) res

appendIrregularRef :: Int -> Int -> Vector Int
appendIrregularRef x y =
  let
    xs = [ [0..i-1] | i <- [0..x-1]]
    ys = [ [0..i-1] | i <- [0..y-1]]
    zs = P.zipWith (P.++) xs ys
    res = concat zs
  in fromList (Z :. P.length res) res

irregularRef :: Int -> Scalar Int
irregularRef n = fromList Z [x]
  where x = P.sum [ P.sum [0..i-1] - P.product [0..n-i-1] | i <- [0..n-1] ]

regularFoldRef :: Shape sh => Array (sh :. Int :. Int) Int -> Array (sh :. Int) Int
regularFoldRef a =
  let
    (sh:.n:.m) = Sugar.shape a
    xs = [[ P.sum [a Sugar.! (Sugar.fromIndex sh j :. k :. i) | k <- [0..n-1]] | j <- [0..Sugar.size sh - 1] ] | i <- [0..m-1]]
  in fromList (sh:.m) (concat xs)

irregularFoldRef :: Int -> Array DIM1 Int
irregularFoldRef n = fromList (Z:.n) $ P.map P.sum $ P.map (P.enumFromTo 0 . (P.subtract 1)) [0..n-1]

test_sequences :: Backend -> Config -> Test
test_sequences backend opt
  = testGroup "sequences"
  $ if backend `elem` supportedBackends
      then test_sequences' backend opt
      else []
  where
    supportedBackends =
      [ Interpreter
#ifdef ACCELERATE_CUDA_BACKEND
      , CUDA
#endif
      ]

test_sequences' :: Backend -> Config -> [Test]
test_sequences' backend opt =
  [ testGroup "id" $ catMaybes
    [
      testIdSequence configInt8   (undefined :: Int8)
    , testIdSequence configInt16  (undefined :: Int16)
    , testIdSequence configInt32  (undefined :: Int32)
    , testIdSequence configInt64  (undefined :: Int64)
    , testIdSequence configWord8  (undefined :: Word8)
    , testIdSequence configWord16 (undefined :: Word16)
    , testIdSequence configWord32 (undefined :: Word32)
    , testIdSequence configWord64 (undefined :: Word64)
    , testIdSequence configFloat  (undefined :: Float)
    , testIdSequence configDouble (undefined :: Double)
    ]
  , testGroup "sum_max" $ catMaybes
    [ testSumMaxSequence configInt8   (undefined :: Int8)
    , testSumMaxSequence configInt16  (undefined :: Int16)
    , testSumMaxSequence configInt32  (undefined :: Int32)
    , testSumMaxSequence configInt64  (undefined :: Int64)
    , testSumMaxSequence configWord8  (undefined :: Word8)
    , testSumMaxSequence configWord16 (undefined :: Word16)
    , testSumMaxSequence configWord32 (undefined :: Word32)
    , testSumMaxSequence configWord64 (undefined :: Word64)
    ]
  , testGroup "logsum" $ catMaybes
    [ testLogsum configFloat  (undefined :: Float)
    , testLogsum configDouble (undefined :: Double)
    ]
  , testGroup "lifted"
    [ testEnumeration
    , testEnumerationIrregular
    , testAppend
    , testAppendIrregular
    , testRegularFold
    , testIrregularFold
    , testIrregular
    ]
  , testGroup "scatter" $ catMaybes
    [ testScatterSequence configInt8   (undefined :: Int8)
    , testScatterSequence configInt16  (undefined :: Int16)
    , testScatterSequence configInt32  (undefined :: Int32)
    , testScatterSequence configInt64  (undefined :: Int64)
    , testScatterSequence configWord8  (undefined :: Word8)
    , testScatterSequence configWord16 (undefined :: Word16)
    , testScatterSequence configWord32 (undefined :: Word32)
    , testScatterSequence configWord64 (undefined :: Word64)
    , testScatterSequence configFloat  (undefined :: Float)
    , testScatterSequence configDouble (undefined :: Double)
    ]
  -- , testGroup "logsum_chunked" $ catMaybes
  --  [ testLogsumChunked configFloat  (undefined :: Float)
  --  , testLogsumChunked configDouble (undefined :: Double)
  --  ]
  -- , testGroup "nested"
  --   [ testNestedSequence
  --   , testNestedIrregularSequence
  --   , testDeepNestedSequence
  --   ]
  ]
  where
    testIdSequence
        :: forall a. (Similar a, A.Num a, Arbitrary a)
        => (Config :-> Bool)
        -> a
        -> Maybe Test
    testIdSequence ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testGroup (show (typeOf (undefined :: a)))
          [ testDim (undefined :: DIM1)
          , testDim (undefined :: DIM2)
          -- , testDim (undefined :: DIM3)
          ]
      where
        testDim :: forall sh. (sh ~ FullShape sh, Slice sh, Shape sh, P.Eq sh, Arbitrary sh, Arbitrary (Array (sh :. Int) a)) => (sh :. Int) -> Test
        testDim sh = testProperty ("DIM" P.++ show (rank sh))
          ((\ xs -> normalise (run1 backend idSequence xs) ~?= normalise (idSequenceRef xs)) :: Array (sh :. Int) a -> Property)


    testSumMaxSequence
        :: forall a. (P.Num a, P.Bounded a, P.Ord a, A.Num a, A.Bounded a, A.Ord a, Similar a, Arbitrary a)
        => (Config :-> Bool)
        -> a
        -> Maybe Test
    testSumMaxSequence ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          ((\xs -> run1 backend sumMaxSequence xs ~?= sumMaxSequenceRef xs) :: Vector a -> Property)


    testScatterSequence
        :: forall a. (P.Num a, A.Num a, Similar a, Arbitrary a)
        => (Config :-> Bool)
        -> a
        -> Maybe Test
    testScatterSequence ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          ((\input ->  arraySize (arrayShape (P.fst input)) P.> 0
                   ==> run1 backend scatterSequence input ~?= scatterSequenceRef input) :: (Vector a, Vector (Int, a)) -> Property)


    testLogsum
        :: forall a. (P.Floating a, A.Floating a, A.FromIntegral Int a, Similar a, Arbitrary a)
        => (Config :-> Bool)
        -> a
        -> Maybe Test
    testLogsum ok _
      | P.not (get ok opt)      = Nothing
      | otherwise               = Just $ testProperty (show (typeOf (undefined :: a)))
          (\ (NonNegative n) -> (run backend (logsum n) :: Scalar a) ~?= logsumRef n)

    -- testNestedSequence :: Test
    -- testNestedSequence =
    --   testProperty "regular"
    --     (\ (NonNegative n) (NonNegative m) -> (run backend (nestedSequence n m) ~?= nestedSequenceRef n m))
    --
    -- testNestedIrregularSequence :: Test
    -- testNestedIrregularSequence =
    --   testProperty "irregular"
    --     (\ (NonNegative n) -> (run backend (nestedIrregularSequence n) ~?= nestedIrregularSequenceRef n))
    --
    -- testDeepNestedSequence :: Test
    -- testDeepNestedSequence =
    --   testProperty "deep"
    --     (\ (NonNegative n) -> (run backend (deepNestedSequence n) ~?= deepNestedSequenceRef n))

    testEnumeration :: Test
    testEnumeration =
      testProperty "enumeration"
      (\ (NonNegative n) (NonNegative x) -> (run backend (enumeration n x) ~?= enumerationRef n x))

    testEnumerationIrregular :: Test
    testEnumerationIrregular =
      testProperty "enumeration-irregular"
      (\ (NonNegative n) -> (run backend (enumerationIrregular n) ~?= enumerationIrregularRef n))

    testIrregular :: Test
    testIrregular =
      testProperty "irregular"
        (\ (NonNegative n) -> (run backend (irregular n) ~?= irregularRef n))

    testAppend :: Test
    testAppend =
      testProperty "append"
        (\ input -> (run1 backend append input ~?= appendRef input))

    testAppendIrregular :: Test
    testAppendIrregular =
      testProperty "append-irregular"
        (\ (NonNegative x) (NonNegative y) -> (run backend (appendIrregular x y) ~?= appendIrregularRef x y))

    testRegularFold :: Test
    testRegularFold = testGroup "fold-regular"
        [ testDim (undefined :: DIM0)
        -- , testDim (undefined :: DIM1)
        ]
      where
        testDim :: forall sh. (Shape sh, Arbitrary (Array (sh:.Int:.Int) Int), P.Eq sh) => sh -> Test
        testDim sh =
          testProperty ("DIM" P.++ show (rank sh))
            ((\input -> normalise (run1 backend regularFold input) ~?= normalise (regularFoldRef input)) :: Array (sh :. Int :. Int) Int -> Property)

    testIrregularFold :: Test
    testIrregularFold =
      testProperty "fold-irregular"
        (\ (NonNegative n) -> (run backend (irregularFold n) ~?= irregularFoldRef n))
