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
  recordInstrumentation
  ) where

import Renovate.Instrument.Monad

