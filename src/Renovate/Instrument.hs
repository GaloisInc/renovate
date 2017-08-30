-- | Helpers for composing binary instrumentors
module Renovate.Instrument (
  Instrument,
  InstrumentInfo(..),
  InstrumentationSite(..),
  runInstrument,
  lookupBlockCFG,
  lookupEntryAddress,
  newGlobalVar,
  lookupGlobalVar,
  recordInstrumentation,
  identity,
  compose
  ) where

import Renovate.BasicBlock
import Renovate.Instrument.Monad

-- | Compose a list of instrumentation functions into a single
-- function suitable for use as an argument to 'redirect'
--
-- The instrumentors are applied in order; that order must be
-- carefully chosen, as the instrumentors are not isolated from each
-- other.
compose :: (Monad m)
        => [SymbolicBlock i a w -> m [TaggedInstruction i a]]
        -> (SymbolicBlock i a w -> m [TaggedInstruction i a])
compose funcs = go funcs
  where
    go [] b = return $ basicBlockInstructions b
    go (f:fs) b = do
      is <- f b
      go fs b { basicBlockInstructions = is }

identity :: (Monad m) => SymbolicBlock i a w -> m [TaggedInstruction i a]
identity sb = return (basicBlockInstructions sb)
