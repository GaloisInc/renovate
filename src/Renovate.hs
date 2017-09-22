-- | Renovate provides an interface for analyzing and rewriting binaries.
--
-- Rewriting actions are specified through the 'C.Rewriter' record; callers must
-- provide a different rewriting backend for each architecture they need to
-- support.
--
-- The current interface allows for rewriting binaries in ELF format.  In the
-- future, other backends will be added, along with a binary format agnostic
-- entry point.
--
-- A typical use currently looks like:
--
-- > import Renovate
-- > rewriteMyElf someElf myRewriter = do
-- >   withElfConfig someElf myRewriter $ \cfg e mem -> do
-- >     case rewriteElf cfg e mem Compact of
-- >       Right (newElf, rewriterInfo) -> writeElfAndInfo newElf rewriterInfo
module Renovate
( -- * Configuration
  C.Rewriter(..),
  C.Analysis(..),
  LB.LayoutStrategy(..),
  LB.CompactOrdering(..),
  C.RenovateConfig(..),
  -- * ELF entry point
  E.withElfConfig,
  E.rewriteElf,
  E.analyzeElf,
  -- * Basic Blocks
  B.SymbolicBlock,
  B.ConcreteBlock,
  B.BasicBlock(..),
  B.SymbolicInfo(..),
  Recovery.BlockInfo(..),
  B.instructionAddresses,
  B.concreteBlockSize,
  B.symbolicBlockSize,
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
  RW.RewriteM,
  RW.recordRewrite,
  RW.lookupGlobalVar,
  RW.newGlobalVar,
  RW.lookupEntryAddress,
  RW.lookupBlockCFG,
  C.compose,
  C.identity,
  C.nop,
  -- * ABI
  ABI.ABI(..),
  ISA.ISA(..),
  ISA.JumpType(..),
  ISA.JumpCondition(..),
  ISA.TrapPredicate(..),
  -- * Results
  RW.RewriteInfo(..),
  RW.RewriteSite(..),
  E.SomeBlocks(..),
  E.RewriterInfo(..),
  D.Diagnostic(..),
  D.Diagnostics(..),
  -- * Constraints
  Recovery.ArchBits,
  ISA.InstructionConstraints,
  -- * Exceptions
  A.BlockAssemblyException(..)
)
where

import qualified Renovate.ABI as ABI
import qualified Renovate.Address as A
import qualified Renovate.Analysis.FunctionRecovery as FR
import qualified Renovate.BasicBlock as B
import qualified Renovate.Config as C
import qualified Renovate.Diagnostic as D
import qualified Renovate.BinaryFormat.ELF as E
import qualified Renovate.ISA as ISA
import qualified Renovate.Redirect.LayoutBlocks as LB
import qualified Renovate.Rewrite as RW
import qualified Renovate.Recovery as Recovery
import qualified Renovate.BasicBlock.Assemble as A
