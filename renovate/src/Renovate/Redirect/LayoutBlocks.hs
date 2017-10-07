{-# LANGUAGE FlexibleContexts #-}
-- | Define the strategy for laying out 'SymbolicBlock's
module Renovate.Redirect.LayoutBlocks (
  layoutBlocks,
  LayoutStrategy(..),
  CompactOrdering(..)
  ) where

import qualified Data.Traversable as T

import qualified Data.Macaw.Memory as MM

import           Renovate.Address
import           Renovate.Redirect.Monad
import           Renovate.Redirect.LayoutBlocks.Compact ( compactLayout )
import           Renovate.Redirect.LayoutBlocks.Types ( LayoutStrategy(..)
                                                      , CompactOrdering(..)
                                                      , SymbolicPair
                                                      , AddressAssignedPair )
import           Renovate.ISA

-- | Compute a concrete address for each 'SymbolicBlock'.
--
-- Right now, we use an inefficient encoding of jumps.  We could do
-- better later on.
layoutBlocks :: (Monad m, T.Traversable t, InstructionConstraints i a, MM.MemWidth w)
             => LayoutStrategy
             -> RelAddress w
             -- ^ Address to begin block layout of instrumented blocks
             -> t (SymbolicPair i a w)
             -> RewriterT i a w m (t (AddressAssignedPair i a w))
layoutBlocks strat startAddr blocks =
  compactLayout startAddr strat blocks
