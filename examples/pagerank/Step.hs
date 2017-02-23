
module Step (

  Update, PageGraph,

  stepRank,
  stepRankSeq,

) where

import Page
import Data.Array.Accelerate                    as A

type PageGraph = Vector Link

type Update = (PageId, Rank)

-- | Find the page rank contribution of one edge in the page graph.
contribution
        :: Acc (Vector Int)    -- ^ Number of outgoing links for each page.
        -> Acc (Vector Rank)   -- ^ Old ranks vector.
        -> Exp Link   -- ^ A link.
        -> Exp Rank  -- ^ New rank.
contribution sizes ranks link
  = let (from, _) = unlift link :: (Exp PageId, Exp PageId)
    in ranks ! index1 (A.fromIntegral from) / A.fromIntegral (sizes ! index1 (A.fromIntegral from))

-- | Updates a vector of ranks by a given vector of updates.
addUpdates
        :: Acc (Vector Link)
        -> Acc (Vector Rank)   -- ^ Old partial ranks.
        -> Acc (Vector Rank)   -- ^ Updates.
        -> Acc (Vector Rank)   -- ^ New partial ranks.
addUpdates links parRanks updates
 = let
     to = A.map A.snd links
   in A.permute (+) parRanks (index1 . A.fromIntegral . (to !)) updates

stepRankSeq :: PageGraph
            -> Acc (Vector Int)  -- Sizes.
            -> Acc (Vector Rank) -- Initial ranks.
            -> Acc (Vector Rank) -- Final ranks.
stepRankSeq p sizes ranks
  = let
      zeroes :: Acc (Vector Rank)
      zeroes = A.fill (shape ranks) 0.0

      -- Ignore shape vector.
      addUpdates' :: Acc (Vector Rank) -> Acc (Vector Z) -> Acc (Vector Link) -> Acc (Vector Rank)
      addUpdates' ranks' _ links = addUpdates links ranks' (A.map (contribution sizes ranks) links)

    in A.collect
     $ A.foldSeqFlatten addUpdates' zeroes
     $ A.toSeqInner (use p)
         -- (A.toSeq (constant (Z :. Split)) (use p))   -- TLM: ??

-- | Perform one iteration step for the internal Page Rank algorithm.
stepRank
        :: Acc PageGraph       -- ^ Part of the pages graph.
        -> Acc (Vector Int)    -- ^ Number of outgoing links for each page.
        -> Acc (Vector Rank)   -- ^ Old ranks vector.
        -> Acc (Vector Rank)   -- ^ Partial ranks vector
        -> Acc (Vector Rank)   -- ^ New ranks vector.

stepRank links sizes ranks parRanks
  = let
        -- pageCount  = A.size sizes

        -- For every link, calculate its contribution to the page it points to
        contrib = A.map (contribution sizes ranks) links

        -- Add to the partial ranks the contribution of the supplied links.
        ranks'  = A.permute (+) parRanks p contrib

        p ix    = let (_, to) = unlift (links ! ix)          :: (Exp PageId, Exp PageId)
                  in index1 (A.fromIntegral to :: Exp Int)

    in ranks'

