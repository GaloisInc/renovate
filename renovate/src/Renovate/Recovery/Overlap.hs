{-# LANGUAGE GADTs #-}
-- | A module defining types and utilities for computing which blocks overlap
module Renovate.Recovery.Overlap (
  BlockRegions,
  blockRegions,
  disjoint
  ) where

import           Control.Lens ( (^.) )
import qualified Data.Foldable as F
import qualified Data.IntervalMap.Strict as IM
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MC
import qualified Data.Map as M
import           Data.Maybe ( fromMaybe )
import           Data.Parameterized.Some ( Some(..) )


import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA

data BlockRegions arch =
  BlockRegions { brIntervals :: !(IM.IntervalMap (ConcreteAddress arch) (Some (MC.ParsedBlock arch)))
               }

-- | Construct a map of overlapping blocks in the binary from macaw discovery results
blockRegions :: ( w ~ MC.ArchAddrWidth arch
                , MC.MemWidth w
                )
             => MC.Memory (MC.ArchAddrWidth arch)
             -> MC.DiscoveryState arch
             -> BlockRegions arch
blockRegions mem di =
  BlockRegions { brIntervals = F.foldl' (addBlock mem) IM.empty discoveredBlocks
               }
  where
    discoveredBlocks = [ Some pb
                       | Some dfi <- M.elems (di ^. MC.funInfo)
                       , pb <- M.elems (dfi ^. MC.parsedBlocks)
                       ]

addBlock :: ( w ~ MC.ArchAddrWidth arch
            , MC.MemWidth w
            )
         => MC.Memory (MC.ArchAddrWidth arch)
         -> IM.IntervalMap (ConcreteAddress arch) (Some (MC.ParsedBlock arch))
         -> Some (MC.ParsedBlock arch)
         -> IM.IntervalMap (ConcreteAddress arch) (Some (MC.ParsedBlock arch))
addBlock mem im (Some pb) = fromMaybe im $ do
  blockStart <- concreteFromSegmentOff mem (MC.pblockAddr pb)
  let blockEnd = blockStart `addressAddOffset` fromIntegral (MC.blockSize pb)
  let i = IM.IntervalCO blockStart blockEnd
  -- Note that the interval-map insert function overwrites the existing value if
  -- the key is already in the map.  This can arise for us because blocks can
  -- appear in more than one function.
  --
  -- NOTE: it isn't actually a problem for us, as we only really care about
  -- cases where blocks overlap but are not identical.
  return (IM.insert i (Some pb) im)

-- | Check if a block is disjoint from all other blocks in the binary
--
-- > disjoint regions b
--
-- returns True if @b@ does not overlap with any other discovered block in @regions@.
disjoint :: ( w ~ MC.ArchAddrWidth arch
            , MC.MemWidth w
            )
         => ISA arch
         -> BlockRegions arch
         -> ConcreteBlock arch
         -> Bool
disjoint isa (BlockRegions im) cb =
  case IM.size (IM.intersecting im i) of
    0 -> error ("No region contains block at address " ++ show baddr)
    1 -> True
    _ -> False
  where
    blockSize = concreteBlockSize isa cb
    baddr = basicBlockAddress cb
    i = IM.IntervalCO baddr (baddr `addressAddOffset` fromIntegral blockSize)
