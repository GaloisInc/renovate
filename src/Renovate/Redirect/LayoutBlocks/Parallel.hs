{-# LANGUAGE FlexibleContexts #-}
-- | Define the strategy for laying out 'SymbolicBlock's in parallel
module Renovate.Redirect.LayoutBlocks.Parallel (
  parallelLayout
  ) where

import qualified Data.Traversable as T

import qualified Data.Macaw.Memory as MM

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Redirect.Monad

-- | Compute a concrete address for each 'SymbolicBlock'.
--
-- Right now, we use an inefficient encoding of jumps.  We could do
-- better later on.
parallelLayout :: (Monad m, T.Traversable t, MM.MemWidth w)
               => RelAddress w
               -- ^ Address to begin block layout of instrumented blocks
               -> t (ConcreteBlock i w, SymbolicBlock i a w)
               -> RewriterT i a w m (t (ConcreteBlock i w, SymbolicBlock i a w, RelAddress w))
parallelLayout startAddr blocks = do
  isa <- askISA
  return $ snd $ T.mapAccumL (assignConcreteAddress isa) startAddr blocks

-- | Compute the concrete address of a block and determine the start
-- address of the successor block.  This is meant to be used in a
-- fold.
assignConcreteAddress :: (MM.MemWidth w)
                      => ISA i a w
                      -> RelAddress w
                      -> (ConcreteBlock i w, SymbolicBlock i a w)
                      -> (RelAddress w, (ConcreteBlock i w, SymbolicBlock i a w, RelAddress w))
assignConcreteAddress isa startAddr (cb, sb) =
  (nextBlockStart, (cb, sb, startAddr))
  where
    nextBlockStart = startAddr `addressAddOffset` fromIntegral (symbolicBlockSize isa startAddr sb)

