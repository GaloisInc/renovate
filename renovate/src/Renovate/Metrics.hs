{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Renovate.Metrics (
    countIncompleteBlocks
  , transitivelyIncompleteBlocks
  , incompleteFunctions
  ) where

import           Control.Lens ( (^.), to )
import           Control.Monad ( guard )
import qualified Data.Foldable as F
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.CFG as MM
import qualified Data.Map as M
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Set as S

import qualified Renovate.Core.Address as RA
import qualified Renovate.Recovery as R

-- | Given a 'R.BlockInfo', compute the total number of discovered blocks that are incomplete
-- (either due to a classification failure or translation error).
--
-- Note that this is different from the number of blocks that are "transitively incomplete" by
-- virtue of being in the same function as another incomplete block.
countIncompleteBlocks :: R.BlockInfo arch -> Int
countIncompleteBlocks =
  sum . map countIncomplete . map snd . M.elems . R.biFunctions
  where
    countIncomplete (Some dfi) =
      let blocks = dfi ^. MD.parsedBlocks . to M.toList
      in sum (map incompleteBlock blocks)
    incompleteBlock (_, b) =
      case MD.pblockTermStmt b of
        MD.ParsedTranslateError {} -> 1
        MD.ClassifyFailure {} -> 1
        _ -> 0

incompleteFunctions :: forall arch
                     . (MM.MemWidth (MM.ArchAddrWidth arch))
                    => MM.Memory (MM.ArchAddrWidth arch)
                    -> R.BlockInfo arch
                    -> M.Map (RA.ConcreteAddress arch) (S.Set (RA.ConcreteAddress arch))
incompleteFunctions mem = F.foldl' addIncompleteFunc M.empty . M.toList . R.biFunctions
  where
    addIncompleteFunc m (faddr, (_, Some dfi))
      | Just incBlkAddrs <- incompleteBlocks (dfi ^. MD.parsedBlocks . to M.toList) =
          M.insert faddr incBlkAddrs m
      | otherwise = m
    incompleteBlocks bs = do
      let addrs = F.foldl' addIfIncompleteBlock S.empty bs
      guard (not (S.null addrs))
      return addrs
    addIfIncompleteBlock :: S.Set (RA.ConcreteAddress arch)
                         -> (unused, MD.ParsedBlock arch ids)
                         -> S.Set (RA.ConcreteAddress arch)
    addIfIncompleteBlock s (_, b)
      | Just caddr <- RA.concreteFromSegmentOff mem (MD.pblockAddr b)  =
          case MD.pblockTermStmt b of
            MD.ParsedTranslateError {} -> S.insert caddr s
            MD.ClassifyFailure {} -> S.insert caddr s
            _ -> s
      | otherwise = s


-- | Count the number of blocks that reside in functions containing incomplete blocks (i.e., the
-- number of blocks that cannot be instrumented due to incomplete analysis)
transitivelyIncompleteBlocks :: R.BlockInfo arch -> S.Set (RA.ConcreteAddress arch)
transitivelyIncompleteBlocks = R.biIncomplete
