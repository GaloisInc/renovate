{-# LANGUAGE FlexibleContexts #-}
-- | Define the strategy for laying out 'SymbolicBlock's
module Renovate.Redirect.LayoutBlocks (
  Layout(..),
  layoutBlocks,
  LayoutStrategy(..),
  CompactOrdering(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.Traversable as T

import           Renovate.Address
import           Renovate.BasicBlock ( InstructionConstraints )
import           Renovate.Redirect.Monad
import           Renovate.Redirect.LayoutBlocks.Compact ( Layout(..), compactLayout )
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
             -> t (SymbolicAddress arch, BS.ByteString)
             -> RewriterT arch m (Layout AddressAssignedPair arch)
layoutBlocks strat startAddr blocks injectedCode =
  compactLayout startAddr strat blocks injectedCode
