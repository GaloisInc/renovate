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
-- A typical analysis looks something like:
--
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE TypeApplications #-}
-- > import           Data.Functor.Const ( Const(..) )
-- > import qualified Data.ElfEdit as E                   -- (elf-edit)
-- > import qualified Data.Macaw.BinaryLoader as MBL      -- (macaw-loader)
-- > import           Data.Macaw.BinaryLoader.X86 ()      -- (macaw-loader-x86)
-- > import qualified Data.Parameterized.NatRepr as NR    -- (parameterized-utils)
-- > import qualified Lang.Crucible.FunctionHandle as FH  -- (crucible)
-- > import qualified Renovate as R                       -- (renovate)
-- > import qualified Renovate.Arch.PPC as RP             -- (renovate-ppc)
-- > import qualified Renovate.Arch.X86_64 as RX          -- (renovate-x86)
-- >
-- > myAnalysis :: (R.HasAnalysisEnv env) => env arch binFmt -> IO (Const Int arch)
-- > myAnalysis env = do
-- >   let bs = R.biBlocks (R.analysisBlockInfo env)
-- >   return (Const (length bs))
-- >
-- > analysis :: R.AnalyzeOnly arch binFmt (Const Int)
-- > analysis = R.AnalyzeOnly myAnalysis
-- >
-- > analysisConfigs :: [(R.Architecture, R.SomeConfig R.AnalyzeOnly (Const Int))]
-- > analysisConfigs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 analysis))
-- >                   , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 analysis))
-- >                   , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config analysis))
-- >                   ]
-- >
-- > myAnalyzeElf :: E.SomeElf E.Elf -> IO Int
-- > myAnalyzeElf someElf = do
-- >   fha <- FH.newHandleAllocator
-- >   R.withElfConfig someElf analysisConfigs $ \config e loadedBinary -> do
-- >     (res, diags) <- R.analyzeElf config fha e loadedBinary
-- >     print diags
-- >     return (getConst res)
-- >
--
-- Note that the analysis function (@myAnalysis@ in the example) has access to
-- all of the data from the 'HasAnalysisEnv` class, including the ISA, ABI,
-- binary image (see 'MBL.LoadedBinary'), and recovered basic blocks from the
-- input binary.
--
-- An example rewriter looks something like:
--
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE TypeApplications #-}
-- > import           Control.Lens ( (^.) )
-- > import qualified Data.ByteString as BS
-- > import           Data.Functor.Const ( Const(..) )
-- > import qualified Data.ElfEdit as E                   -- (elf-edit)
-- > import qualified Data.Macaw.BinaryLoader as MBL      -- (macaw-loader)
-- > import           Data.Macaw.BinaryLoader.X86 ()      -- (macaw-loader-x86)
-- > import qualified Data.Parameterized.NatRepr as NR    -- (parameterized-utils)
-- > import qualified Lang.Crucible.FunctionHandle as FH  -- (crucible)
-- > import qualified Renovate as R                       -- (renovate)
-- > import qualified Renovate.Arch.PPC as RP             -- (renovate-ppc)
-- > import qualified Renovate.Arch.X86_64 as RX          -- (renovate-x86)
-- >
-- > myAnalysis :: (R.HasAnalysisEnv env) => env arch binFmt -> IO (Const Int arch)
-- > myAnalysis env = do
-- >   let bs = R.biBlocks (R.analysisBlockInfo env)
-- >   return (Const (length bs))
-- >
-- > newtype RewriteState arch = RewriteState (R.SymbolicAddress arch)
-- >
-- > myInit :: (R.HasAnalysisEnv env) => env arch binFmt -> Const Int arch -> R.RewriteM arch (RewriteState arch)
-- > myInit env nBlocks = do
-- >   let fn = BS.pack (replicate (getConst nBlocks) 0xF4)
-- >   addr <- R.injectFunction "rawData" fn
-- >   return (RewriteState addr)
-- >
-- > myRewriter :: (R.HasAnalysisEnv env)
-- >            => env arch binFmt
-- >            -> Const Int arch
-- >            -> RewriteState arch
-- >            -> R.SymbolicBlock arch
-- >            -> R.RewriteM arch (Maybe [R.TaggedInstruction arch (R.InstructionAnnotation arch)])
-- > myRewriter env nBlocks (RewriteState newFuncAddr) symBlock =
-- >   return (Just (R.basicBlockInstructions symBlock))
-- >
-- > analysis :: R.AnalyzeAndRewrite arch binFmt (Const Int)
-- > analysis = R.AnalyzeAndRewrite { R.arAnalyze = myAnalysis
-- >                                , R.arInitializeRewriter = myInit
-- >                                , R.arRewrite = myRewriter
-- >                                }
-- >
-- > analysisConfigs :: [(R.Architecture, R.SomeConfig R.AnalyzeAndRewrite (Const Int))]
-- > analysisConfigs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 analysis))
-- >                   , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 analysis))
-- >                   , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config analysis))
-- >                   ]
-- >
-- > myAnalyzeElf :: E.SomeElf E.Elf -> IO Int
-- > myAnalyzeElf someElf = do
-- >   fha <- FH.newHandleAllocator
-- >   R.withElfConfig someElf analysisConfigs $ \config e loadedBinary -> do
-- >     (newElf, res, ri) <- R.rewriteElf config fha e loadedBinary R.Parallel
-- >     print (getConst res)
-- >     print (ri ^. R.riBlockMapping)
-- >     return (getConst res)
--
-- The result of the analysis pass (@myAnalysis@ in the example) feeds into the
-- rewriter initializer (@myInit@ in the example), whose purpose is to allocate
-- global data (in the 'RW.RewriteM' monad) for use during the rewriting pass.
-- The actual rewriting pass (@myRewriter@ in the example), which modifies one
-- block at a time.  Note that the rewriter can return 'Nothing' for a block,
-- which tells renovate to not modify the block at all.
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
  Recovery.ArchInfo(..),
  Recovery.ArchVals(..),
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
  ISA.TrapPredicate(..),
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
  Recovery.ArchBits,
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
