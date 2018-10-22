{-# LANGUAGE FlexibleContexts #-}
-- | Define the strategy for laying out 'SymbolicBlock's
module Renovate.Redirect.LayoutBlocks (
  layoutBlocks,
  LayoutStrategy(..),
  CompactOrdering(..)
  ) where

import qualified Data.Traversable as T

import           Renovate.Address
import           Renovate.BasicBlock ( InstructionConstraints, ConcreteBlock )
import           Renovate.Redirect.Monad
import           Renovate.Redirect.LayoutBlocks.Compact ( compactLayout )
import           Renovate.Redirect.LayoutBlocks.Types ( LayoutStrategy(..)
                                                      , CompactOrdering(..)
                                                      , SymbolicPair
                                                      , AddressAssignedPair )

-- | Compute a concrete address for each 'SymbolicBlock'.
--
-- Right now, we use an inefficient encoding of jumps.  We could do
-- better later on.
layoutBlocks :: (Monad m, T.Traversable t, InstructionConstraints arch)
             => LayoutStrategy
             -> ConcreteAddress arch
             -- ^ Address to begin block layout of instrumented blocks
             -> t (SymbolicPair arch)
             -> RewriterT arch m ([AddressAssignedPair arch], [ConcreteBlock arch])
layoutBlocks strat startAddr blocks =
  compactLayout startAddr strat blocks
