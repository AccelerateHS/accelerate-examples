{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Rank (rank)
  where

import Count
import Load
import Page
import Step
import Time

import Control.Monad
import Prelude                                          as P
import qualified Data.Vector.Storable                   as S

import Data.Array.Accelerate                            as A
import Data.Array.Accelerate.Array.Sugar                as A ( EltRepr )
import Data.Array.Accelerate.Examples.Internal          as A
import Data.Array.Accelerate.IO                         as A



arrayize
    :: (Vectors (EltRepr e) ~ S.Vector a, Elt e, S.Storable a)
    => S.Vector a
    -> Vector e
arrayize vec = (A.fromVectors (Z :. S.length vec) vec)

-- | Perform some iterations of the PageRank algorithm by loading the whole
--   links graph into memory and passes it off in chunks to Accelerate to
--   avoid running out of device memory.
rank
    :: Backend
    -> Bool                 -- ^ Do not use Accelerate sequencing
    -> Int                  -- ^ Number of iterations to run.
    -> FilePath             -- ^ Path to links file.
    -> FilePath             -- ^ Path to titles file.
    -> IO ()
rank backend useSeq steps pagesPath titlesPath
 = do   (_, maxPageId)        <- countPages pagesPath
        putStrLn "* Loading pages."
        (!from, !to, !sizes)  <- loadPages pagesPath (P.fromIntegral maxPageId)
        -- let edgeCount   = S.length from
        let !pageCount  = S.length sizes
        let !ranks      = initialRanks backend pageCount
        timed $ pageRank backend useSeq steps pageCount from to (arrayize sizes) titlesPath ranks
        putStr "\n"
        return ()

-- | Construct the initial ranks vector.
initialRanks :: Backend -> Int -> A.Vector Rank
initialRanks backend pageCount
 = let  !startRank   = 1 / P.fromIntegral pageCount
   in   run backend $ A.fill (index1 (A.lift pageCount)) startRank


-- | Run several iterations of the internal PageRank algorithm.
pageRank
        :: Backend
        -> Bool                 -- ^ Do not use Accelerate sequencing.
        -> Int                  -- ^ Number of iterations to run.
        -> Int                  -- ^ Number of pages
        -> S.Vector PageId      -- ^ Pages graph from.
        -> S.Vector PageId      -- ^ Pages graph to (same length as from).
        -> A.Vector Int         -- ^ The degree of each page
        -> FilePath             -- ^ Path to titles file.
        -> A.Vector Rank        -- ^ Initial ranks.
        -> IO ()

pageRank backend useSeq maxIters pageCount from to sizes0 _titlesFile ranks0 =
  go maxIters ranks0
  where
        go :: Int -> A.Vector Rank -> IO ()
        go 0 !ranks
         = let !rankMaxIx       = maxIndex ranks
               !rankMax         = indexArray ranks (Z:.indexArray rankMaxIx Z)
               -- Sum up the ranks for all the pages,
               -- this should be very close to 1, minus some some round-off error.
           in do   -- Show the page with the maximum rank.
                  putStrLn $ "  high ix    : "  P.++ show (indexArray rankMaxIx Z)
                  putStrLn $ "  high rank  : "  P.++ show rankMax
                  return ()

        go !i !ranks
         = do   putStr "\n"
                putStrLn $ "* Step " P.++ show i

                -- Run a step of the algorithm.
                let ranks1 = if useSeq then stepInSeq ranks
                                       else step ranks
                -- let ranks1 = stepInChunks ranks zeros 0
                let ranks2 = addDangles (ranks1, sizes0)

                -- Sum up the ranks for all the pages,
                -- this should be very close to 1, minus some some round-off error.
                -- let rankSum = run1 backend A.sum ranks2
                putStrLn $ "  rank sum   : "  P.++ show (indexArray (rankSum ranks2) Z)

                go (i - 1) ranks2

        -- Sum up the ranks for all the pages,
        -- this should be very close to 1, minus some some round-off error.
        rankSum :: Vector Rank -> Scalar Rank
        rankSum = run1 backend A.sum

        -- Add to the rank the score due to dangling vectors.
        addDangles :: (Vector Rank, Vector Int) -> Vector Rank
        addDangles = run1 backend $ A.uncurry $ \ranks sizes -> let

            dangleScore = A.fold (+) 0 (A.zipWith d ranks sizes)

            dangleContrib :: Acc (Scalar Rank)
            dangleContrib = A.unit (the dangleScore / (A.lift (P.fromIntegral pageCount :: Float)))

            d r s = s A.== 0 ? (r, 0)

          in A.map (+ A.the dangleContrib) ranks

        stepInSeq :: A.Vector Rank -> A.Vector Rank
        stepInSeq =
          let !pages = A.fromVectors (Z :. S.length from) (((), from), to)
          in  run1 backend (stepRankSeq pages (use sizes0))

        edgeCount = S.length from

        zeros :: Vector Rank
        zeros = run backend $ A.fill (A.lift $ Z :. pageCount) 0

        pageGraph :: PageGraph
        pageGraph = (fromVectors (Z:.edgeCount) (((),from),to))

        step :: Vector Rank -> Vector Rank
        step = run1 backend $ \ranks -> stepRank (use pageGraph) (use sizes0) ranks (use zeros)

        -- Computer the index of the maximum rank.
        maxIndex :: A.Vector Rank -> A.Scalar Int
        maxIndex = run1 backend $ (\ranks -> A.fold (\x y -> ranks ! index1 x A.> ranks ! index1 y ? (x,y)) 0 (A.enumFromN (A.shape ranks) 0))

