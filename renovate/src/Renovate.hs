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
  RCL.LayoutStrategy(..),
  RCL.Grouping(..),
  RCL.Allocator(..),
  RCL.TrampolineStrategy(..),
  RCL.CompactOrdering(..),
  C.AnalyzeOnly(..),
  C.AnalyzeAndRewrite(..),
  B.ModifiedInstructions(..),
  C.HasAnalysisEnv(..),
  C.HasSymbolicBlockMap(..),
  C.RenovateConfig(..),
  -- * ELF entry point
  E.withElfConfig,
  E.rewriteElf,
  E.analyzeElf,
  -- * Basic Blocks
  -- ** Concrete blocks
  B.ConcreteBlock,
  B.concreteBlock,
  B.concreteBlockAddress,
  B.withConcreteInstructions,
  B.concreteDiscoveryBlock,
  -- ** Symbolic blocks
  B.SymbolicBlock,
  B.symbolicBlock,
  B.symbolicBlockOriginalAddress,
  B.symbolicBlockSymbolicAddress,
  B.symbolicBlockSymbolicSuccessor,
  B.symbolicBlockDiscoveryBlock,
  B.withSymbolicInstructions,
  -- ** Concretized blocks
  B.ConcretizedBlock,
  B.concretizedBlock,
  B.concretizedBlockAddress,
  B.withConcretizedInstructions,
  -- ** Block helpers
  B.HasConcreteAddresses,
  B.blockAddress,
  B.blockSize,
  B.withInstructionAddresses,
  B.instructionAddresses',
  -- ** Others
  Recovery.BlockInfo(..),
  Recovery.isIncompleteBlockAddress,
  Recovery.numBlockRegions,
  Recovery.SymbolicCFG,
  Recovery.SymbolicRegCFG,
  Recovery.getSymbolicCFG,
  Recovery.getSymbolicRegCFG,
  B.symbolicBlockSize,
  -- * Instructions
  RCI.InstructionArchRepr,
  RCI.InstructionArchReprKind,
  RCI.SomeInstructionArchRepr(..),
  RCI.Instruction,
  RCI.RegisterType,
  RCI.ToGenericInstruction(..),
  RCR.ArchitectureRelocation,
  RCR.Relocation(..),
  RCI.InstructionConstraints(..),
  -- * Addresses
  RCA.SymbolicAddress,
  RCA.ConcreteAddress,
  RCA.stableAddress,
  RCA.absoluteAddress,
  RCA.concreteFromSegmentOff,
  RCA.concreteAsSegmentOff,
  RCA.concreteFromAbsolute,
  RCA.addressAddOffset,
  RCA.addressDiff,
  B.SymbolicInfo(..),
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
  RW.recordLogMsg,
  RW.injectFunction,
  RW.injectInstructions,
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
  ISA.isaDefaultInstructionArchRepr,
  ISA.JumpType(..),
  ISA.JumpCondition(..),
  ISA.HasModifiableTarget,
  ISA.NoModifiableTarget,
  ISA.StackAddress(..),
  -- * Results
  RRe.BlockMapping(..),
  RRe.RedirectionResult(..),
  E.RewriterInfo(..),
  E.SectionInfo(..),
  RCL.RewritePair(..),
  -- * Constraints
  ISA.ArchConstraints,
  -- * Diagnostics
  Diagnostic.Diagnostic(..),
  Diagnostic.ELFDiagnostic(..),
  Diagnostic.RedirectionDiagnostic(..),
  Diagnostic.RecoveryDiagnostic(..),
  -- * Exceptions
  RCE.RenovateException(..)
)
where

import qualified Renovate.ABI as ABI
import qualified Renovate.Arch as Arch
import qualified Renovate.Core.Address as RCA
import qualified Renovate.Core.BasicBlock as B
import qualified Renovate.Core.Diagnostic as Diagnostic
import qualified Renovate.Core.Exception as RCE
import qualified Renovate.Core.Instruction as RCI
import qualified Renovate.Core.Layout as RCL
import qualified Renovate.Core.Relocation as RCR
import qualified Renovate.Analysis.FunctionRecovery as FR
import qualified Renovate.Config as C
import qualified Renovate.BinaryFormat.ELF as E
import qualified Renovate.ISA as ISA
import qualified Renovate.Rewrite as RW
import qualified Renovate.Redirect as RRe
import qualified Renovate.Recovery as Recovery
