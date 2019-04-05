{-# LANGUAGE FlexibleContexts #-}
-- | Define the strategy for laying out 'SymbolicBlock's
module Renovate.Redirect.LayoutBlocks (
  Layout(..),
  layoutBlocks,
  LayoutStrategy(..),
  Grouping(..),
  grouping,
  CompactOrdering(..)
  ) where

import qualified Data.ByteString as BS
import           Control.Monad.IO.Class ( MonadIO )
import           Data.Map ( Map )
import qualified Data.Traversable as T

import           Renovate.Address
import           Renovate.BasicBlock ( InstructionConstraints )
import           Renovate.Recovery ( SymbolicCFG )
import           Renovate.Redirect.Monad
import           Renovate.Redirect.LayoutBlocks.Compact ( Layout(..), compactLayout )
import           Renovate.Redirect.LayoutBlocks.Types ( LayoutStrategy(..)
                                                      , Grouping(..)
                                                      , grouping
                                                      , CompactOrdering(..)
                                                      , SymbolicPair
                                                      , AddressAssignedPair )

-- | Compute a concrete address for each 'SymbolicBlock'.
--
-- Right now, we use an inefficient encoding of jumps.  We could do
-- better later on.
layoutBlocks :: (MonadIO m, T.Traversable t, InstructionConstraints arch)
             => LayoutStrategy
             -> ConcreteAddress arch
             -- ^ Address to begin block layout of instrumented blocks
             -> t (SymbolicPair arch)
             -> t (SymbolicAddress arch, BS.ByteString)
             -> Map (ConcreteAddress arch) (SymbolicCFG arch)
             -> RewriterT arch m (Layout AddressAssignedPair arch)
layoutBlocks strat startAddr blocks injectedCode cfgs =
  compactLayout startAddr strat blocks injectedCode cfgs
