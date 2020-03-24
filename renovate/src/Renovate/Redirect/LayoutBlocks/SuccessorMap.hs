{-# LANGUAGE FlexibleContexts #-}
module Renovate.Redirect.LayoutBlocks.SuccessorMap (
  SuccessorMap,
  successorMap,
  lookupSuccessor
  ) where

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Macaw.CFG as MC

import qualified Renovate.Address as RA
import qualified Renovate.BasicBlock as RB
import qualified Renovate.ISA as RI

import           Renovate.Redirect.LayoutBlocks.Types

newtype SuccessorMap arch = SuccessorMap (M.Map (RA.SymbolicAddress arch) (RA.SymbolicAddress arch))

-- | Construct a mapping from symbolic block addresses to the address of the block immediately
-- following them (i.e., the fallthrough successor if the first block does not end in an
-- unconditional jump)
successorMap :: (Foldable t, MC.MemWidth (MC.ArchAddrWidth arch))
             => RI.ISA arch
             -> t (WithProvenance RB.SymbolicBlock arch)
             -> SuccessorMap arch
successorMap isa symPairs =
  SuccessorMap (F.foldl' indexSymbolicSuccessors M.empty symPairs)
  where
    concToSymMap = M.fromList [ ( RB.symbolicBlockOriginalAddress b0
                                , RB.symbolicBlockSymbolicAddress b0
                                )
                              | wp <- F.toList symPairs
                              , let b0 = withProvenance wp
                              ]
    indexSymbolicSuccessors m wp =
      let concBlock = originalBlock wp
          symBlock = withProvenance wp
          symAddr = RB.symbolicBlockSymbolicAddress symBlock
          nextAbsAddr = RB.concreteBlockAddress concBlock `RA.addressAddOffset` fromIntegral (RB.concreteBlockSize isa concBlock)
      in case M.lookup nextAbsAddr concToSymMap of
        Nothing -> M.insert symAddr (RA.StableAddress nextAbsAddr) m
        Just symSuccessor -> M.insert symAddr symSuccessor m

-- | Find the symbolic address of the successor to the input block
--
-- The lookup is done through the 'SuccessorMap'.
lookupSuccessor :: SuccessorMap arch
                -> RB.SymbolicBlock arch
                -> Maybe (RA.SymbolicAddress arch)
lookupSuccessor (SuccessorMap sm) sb =
  M.lookup (RB.symbolicBlockSymbolicAddress sb) sm
