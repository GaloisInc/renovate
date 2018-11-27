-- | Renovate provides an interface for analyzing and rewriting binaries.
--
-- The core library renovate implements architecture-independent binary
-- rewriting in terms of architecture-specific backends that provide
-- configurations ('RenovateConfig').  Examples include renovate-ppc and
-- renovate-x86.  The backends are separated out to reduce the dependencies of
-- the core library, and to allow client code flexibility to only include
-- support for the architectures that are actually required for a given use
-- case.
--
-- The library interface is primarily through up-front configurations that
-- specify analysis and rewriting actions; renovate itself handles selecting the
-- correct configuration for the binary it is given.  For many use cases, the
-- same analysis can be used across multiple instruction set architectures
-- (ISAs).  Rewriting passes are generally architecture-specific.
--
-- The current interface allows for rewriting binaries in ELF format.  In the
-- future, other backends will be added, along with a binary format agnostic
-- entry point.
--
-- See Renovate.Tutorial for examples of using the library.
module Renovate
( -- * Configuration
  Arch.Architecture(..),
  C.SomeConfig(..),
  LB.LayoutStrategy(..),
  LB.CompactOrdering(..),
  C.AnalyzeOnly(..),
  C.AnalyzeAndRewrite(..),
  C.HasAnalysisEnv(..),
  C.HasSymbolicBlockMap(..),
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
  Recovery.isIncompleteBlockAddress,
  Recovery.numBlockRegions,
  Recovery.SymbolicCFG,
  Recovery.SymbolicRegCFG,
  Recovery.getSymbolicCFG,
  Recovery.getSymbolicRegCFG,
  B.instructionAddresses,
  B.instructionAddresses',
  B.concreteBlockSize,
  B.symbolicBlockSize,
  -- * Instructions
  B.TaggedInstruction,
  B.tagInstruction,
  B.projectInstruction,
  B.symbolicTarget,
  B.Instruction,
  B.InstructionAnnotation,
  B.RegisterType,
  B.ToGenericInstruction(..),
  -- * Addresses
  A.SymbolicAddress,
  A.ConcreteAddress,
  A.absoluteAddress,
  A.concreteFromSegmentOff,
  A.concreteAsSegmentOff,
  A.concreteFromAbsolute,
  A.addressAddOffset,
  A.addressDiff,
  -- * Analysis
  -- ** Function Recovery
  FR.recoverFunctions,
  FR.Completion(..),
  FR.FunctionCFG(..),
  -- * Rewriting API
  RW.RewriteM,
  RW.BlockCFGIndex,
  RW.mkRewriteEnv,
  RW.recordRewrite,
  RW.injectFunction,
  RW.newGlobalVar,
  RW.getBlockIndex,
  RW.lookupEntryAddress,
  RW.lookupBlockCFG,
  RW.getABI,
  RW.getISA,
  C.compose,
  C.identity,
  C.nop,
  -- * ABI
  ABI.ABI(..),
  ISA.ISA(..),
  ISA.JumpType(..),
  ISA.JumpCondition(..),
  ISA.StackAddress(..),
  -- * Results
  RW.RewriteInfo(..),
  RW.RewriteSite(..),
  E.SomeBlocks(..),
  E.RewriterInfo,
  E.riSmallBlockCount,
  E.riReusedByteCount,
  E.riUnrelocatableTerm,
  E.riInstrumentationSites,
  E.riInitialBytes,
  E.riAppendedSegments,
  E.riRedirectionDiagnostics,
  E.riBlockRecoveryDiagnostics,
  E.riRecoveredBlocks,
  E.riEntryPointAddress,
  E.riSectionBaseAddress,
  E.riOverwrittenRegions,
  E.riSegmentVirtualAddress,
  E.riOriginalTextSize,
  E.riNewTextSize,
  E.riIncompleteBlocks,
  E.riBlockMapping,
  E.riOutputBlocks,
  D.Diagnostic(..),
  D.Diagnostics(..),
  -- * Constraints
  B.InstructionConstraints,
  -- * Exceptions
  A.BlockAssemblyException(..)
)
where

import qualified Renovate.ABI as ABI
import qualified Renovate.Arch as Arch
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
