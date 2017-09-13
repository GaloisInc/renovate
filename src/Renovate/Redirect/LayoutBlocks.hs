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
import           Renovate.BasicBlock
import           Renovate.Redirect.Monad
import           Renovate.Redirect.LayoutBlocks.Compact ( compactLayout )
import           Renovate.Redirect.LayoutBlocks.Parallel ( parallelLayout )
import           Renovate.Redirect.LayoutBlocks.Types ( LayoutStrategy(..)
                                                      , CompactOrdering(..) )

-- | Compute a concrete address for each 'SymbolicBlock'.
--
-- Right now, we use an inefficient encoding of jumps.  We could do
-- better later on.
layoutBlocks :: (Monad m, T.Traversable t, Show (i a), MM.MemWidth w)
             => LayoutStrategy
             -> MM.Memory w
             -> RelAddress w
             -- ^ Address to begin block layout of instrumented blocks
             -> t (ConcreteBlock i w, SymbolicBlock i a w)
             -> RewriterT i a w m (t (ConcreteBlock i w, SymbolicBlock i a w, RelAddress w))
layoutBlocks strat mem startAddr blocks =
  case strat of
    Parallel -> parallelLayout startAddr blocks
    Compact ordering -> compactLayout mem startAddr ordering blocks
