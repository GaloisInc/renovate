module Renovate
( -- * Configuration
  E.Rewriter(..),
  LB.LayoutStrategy(..),
  E.RenovateConfig,
  -- * ELF entry point
  E.withElfConfig,
  E.rewriteElf,
  -- * Basic Blocks
  B.SymbolicBlock,
  B.ConcreteBlock,
  B.BasicBlock(..),
  B.SymbolicInfo(..),
  -- * Instructions
  B.TaggedInstruction,
  B.tagInstruction,
  B.projectInstruction,
  -- * Addresses
  A.SymbolicAddress,
  A.RelAddress,
  A.absoluteAddress,
  A.relFromSegmentOff,
  A.firstRelAddress,
  A.addressAddOffset,
  A.addressDiff,
  -- * Analysis
  -- ** Function Recovery
  FR.recoverFunctions,
  FR.Completion(..),
  FR.FunctionCFG(..),
  -- * Rewriting API
  I.Instrument,
  I.recordInstrumentation,
  I.lookupGlobalVar,
  I.newGlobalVar,
  I.lookupEntryAddress,
  I.lookupBlockCFG,
  I.compose,
  I.identity,
  -- * ABI
  ABI.ABI(..),
  ISA.ISA(..),
  ISA.TrapPredicate(..),
  -- * Results
  I.InstrumentInfo(..),
  I.InstrumentationSite(..),
  E.SomeBlocks(..),
  E.RewriterInfo(..),
  D.Diagnostic(..),
  D.Diagnostics(..)
)
where

import qualified Renovate.ABI as ABI
import qualified Renovate.Address as A
import qualified Renovate.Analysis.FunctionRecovery as FR
import qualified Renovate.BasicBlock as B
import qualified Renovate.Diagnostic as D
import qualified Renovate.BinaryFormat.ELF as E
import qualified Renovate.Instrument as I
import qualified Renovate.ISA as ISA
import qualified Renovate.Redirect.LayoutBlocks as LB
