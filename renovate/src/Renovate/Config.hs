{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
-- | Internal helpers for the ELF rewriting interface
module Renovate.Config (
  HasAnalysisEnv(..),
  HasSymbolicBlockMap(..),
  AnalysisEnv(..),
  RewriterAnalysisEnv(..),
  AnalyzeOnly(..),
  AnalyzeAndRewrite(..),
  RenovateConfig(..),
  SomeConfig(..),
  compose,
  identity,
  nop
  ) where

import qualified Control.Monad.Catch as C
import           Control.Monad.ST ( ST, RealWorld )
import qualified Data.ByteString as B
import           Data.Map.Strict ( Map )
import           Data.Word ( Word64 )

import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Architecture.Info as MM
import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.Symbolic as MS
import qualified Lang.Crucible.FunctionHandle as C

import qualified Renovate.Address as RA
import qualified Renovate.BasicBlock as B
import qualified Renovate.ABI as ABI
import qualified Renovate.ISA as ISA
import qualified Renovate.Recovery as R
import qualified Renovate.Rewrite as RW

-- | A wrapper around a 'RenovateConfig' that hides parameters and
-- allows us to have collections of configs while capturing the
-- necessary class dictionaries. The 'SomeConfig' is parameterized by
-- a constraint @c@ over the hidden 'RenovateConfig' params and a result type @b@.
--
-- * The @callbacks@ type is used to tell renovate whether it is running a code
--   analysis ('AnalyzeOnly') or a combined analysis + rewriting pass
--   ('AnalyzeAndRewrite').
--
-- * The result type @b@ is the type of results of the pre-rewriting analysis
--   pass.  It is parameterized by the architecture of the analysis in such a
--   way that there can be a list of 'SomeConfig' while still containing
--   architecture-parameterized data (where the architecture is hidden by the
--   existential).
data SomeConfig callbacks (b :: * -> *) = forall arch binFmt
                  . (B.InstructionConstraints arch,
                     MS.ArchBits arch,
                     MS.MacawArchConstraints arch,
                     MBL.BinaryLoader arch binFmt
                    )
                  => SomeConfig (NR.NatRepr (MM.ArchAddrWidth arch)) (MBL.BinaryRepr binFmt) (RenovateConfig arch binFmt callbacks b)

-- | This is the data type instantiated for 'HasAnalysisEnv'.  The goal is to
-- have this as a completely internal type that the user never sees.  Users
-- should only access this data through the interface.
data AnalysisEnv arch binFmt =
  AnalysisEnv { aeLoadedBinary :: MBL.LoadedBinary arch binFmt
              , aeBlockInfo :: R.BlockInfo arch
              , aeISA :: ISA.ISA arch
              , aeABI :: ABI.ABI arch
              , aeHandleAllocator :: C.HandleAllocator RealWorld
              }

-- | Likewise, this type is for implementing 'HasSymbolicBlockMap'
data RewriterAnalysisEnv arch binFmt =
  RewriterAnalysisEnv { raeEnv :: AnalysisEnv arch binFmt
                      , raeSymBlockMap :: Map (RA.ConcreteAddress arch) (B.SymbolicBlock arch)
                      }

instance HasAnalysisEnv AnalysisEnv where
  analysisLoadedBinary = aeLoadedBinary
  analysisBlockInfo = aeBlockInfo
  analysisISA = aeISA
  analysisABI = aeABI
  analysisHandleAllocator = aeHandleAllocator

instance HasAnalysisEnv RewriterAnalysisEnv where
  analysisLoadedBinary = aeLoadedBinary . raeEnv
  analysisBlockInfo = aeBlockInfo . raeEnv
  analysisISA = aeISA . raeEnv
  analysisABI = aeABI . raeEnv
  analysisHandleAllocator = aeHandleAllocator . raeEnv

instance HasSymbolicBlockMap RewriterAnalysisEnv where
  getSymbolicBlockMap = raeSymBlockMap

-- | This class exposes all of the user-facing functionality in the analysis environment.
--
-- Both the analysis and rewriter have access to this information.  It includes all of the
-- basic blocks discovered by the binary analysis, as well as tools for working with them.
--
-- Note that the 'C.HandleAllocator' is included because the basic block
-- recovery uses it to construct Crucible terms (useful for analysis), and some
-- interactions with those terms later require using the same handle allocator
-- used to create them.  Users could save that handle allocator, but it is
-- exposed through this interface for convenience.
class HasAnalysisEnv env where
  analysisLoadedBinary :: env arch binFmt -> MBL.LoadedBinary arch binFmt
  analysisBlockInfo :: env arch binFmt  -> R.BlockInfo arch
  analysisISA :: env arch binFmt -> ISA.ISA arch
  analysisABI :: env arch binFmt -> ABI.ABI arch
  analysisHandleAllocator :: env arch binFmt -> C.HandleAllocator RealWorld

-- | This additional environment is available to the rewriter.  It has
-- information derived from the analysis environment that is useful for
-- rewriting.  In particular, it contains a mapping from concrete addresses to
-- the symbolic addresses that are used in the rewriter.
class HasSymbolicBlockMap env where
  getSymbolicBlockMap :: env arch binFmt -> Map (RA.ConcreteAddress arch) (B.SymbolicBlock arch)

-- | The configuration for a binary analysis
--
-- This is specifically the configuration for an analysis-only pass with no
-- rewriting.  Note that the analysis itself is in IO.
data AnalyzeOnly arch binFmt b =
  AnalyzeOnly { aoAnalyze :: forall env . (HasAnalysisEnv env) => env arch binFmt -> IO (b arch) }

-- | The configuration for a combined analysis and rewriting pass
--
-- This has additional callbacks that are run in 'RW.RewriteM' to enable setup
-- before the analysis and rewriting phases.  Those setup phases are useful for
-- allocating global variables and injecting code in the rewriting context.
--
-- Note that the analysis is still in IO.
data AnalyzeAndRewrite arch binFmt b =
  forall preAnalyzeState rewriterState .
  AnalyzeAndRewrite { arPreAnalyze :: forall env . (HasAnalysisEnv env, HasSymbolicBlockMap env) => env arch binFmt -> RW.RewriteM arch (preAnalyzeState arch)
                    , arAnalyze :: forall env . (HasAnalysisEnv env, HasSymbolicBlockMap env) => env arch binFmt -> preAnalyzeState arch -> IO (b arch)
                    , arPreRewrite :: forall env . (HasAnalysisEnv env, HasSymbolicBlockMap env) => env arch binFmt -> b arch -> RW.RewriteM arch (rewriterState arch)
                    , arRewrite :: forall env . (HasAnalysisEnv env, HasSymbolicBlockMap env) => env arch binFmt -> b arch -> rewriterState arch -> B.SymbolicBlock arch -> RW.RewriteM arch (Maybe [B.TaggedInstruction arch (B.InstructionAnnotation arch)])
                    }

-- | The configuration required for a run of the binary rewriter.
--
-- The binary rewriter is agnostic to the container format of the binary (e.g.,
-- ELF, COFF, Mach-O).  This configuration contains all of the information
-- necessary to analyze and rewrite a binary.
--
-- Note that there is information encoded in the config for specific binary
-- formats (e.g., ELF), but the core binary rewriter is still architecture
-- agnostic.  Those extra helpers are used by the different binary format
-- backends in the core.
--
-- The type parameters are as follows:
--
-- * @arch@ the architecture type tag for the architecture
-- * @binFmt@ is the format of the binary loaded (e.g., ELF, Mach-O)
-- * @callbacks@ is the type of the callback in the configuration (the analysis only frontend and the analysis+rewriter frontend have different callback types)
-- * @b@ (which is applied to @arch@) the type of analysis results produced by the analysis and passed to the rewriter
data RenovateConfig arch binFmt callbacks (b :: * -> *) = RenovateConfig
  { rcISA           :: ISA.ISA arch
  , rcABI           :: ABI.ABI arch
  , rcArchInfo      :: MBL.LoadedBinary arch binFmt -> MM.ArchitectureInfo arch
  -- ^ Architecture info for macaw
  , rcAssembler     :: forall m . (C.MonadThrow m) => B.Instruction arch () -> m B.ByteString
  , rcDisassembler  :: forall m . (C.MonadThrow m) => B.ByteString -> m (Int, B.Instruction arch ())
  , rcBlockCallback :: Maybe (MC.ArchSegmentOff arch -> ST RealWorld ())
  -- ^ A callback called for each discovered block; the argument
  -- is the address of the discovered block
  , rcFunctionCallback :: Maybe (Int, MBL.LoadedBinary arch binFmt -> MC.ArchSegmentOff arch -> R.BlockInfo arch -> IO ())
  -- ^ A callback called for each discovered function.  The
  -- arguments are the address of the discovered function and the
  -- recovery info (a summary of the information returned by
  -- macaw).  The 'Int' is the number of iterations before
  -- calling the function callback.
  , rcAnalysis      :: callbacks arch binFmt b
  -- ^ Caller-specified analysis (and possibly rewriter) to apply to the binary
  , rcMaxUnconditionalJumpSize :: Word64
  -- ^ How far can this architecture's unconditional relative jumps reach? New
  -- code blocks will be laid out in virtual address space within this many
  -- bytes of the original code blocks, so that the two can jump to each other
  -- as necessary.
  , rcDataLayoutBase :: Word64
  -- ^ The base address to start laying out new data
  , rcUpdateSymbolTable :: Bool
  -- ^ True if the symbol table should be updated; this is a
  -- temporary measure.  Our current method for updating the
  -- symbol table does not work for PowerPC, so we don't want to
  -- do it there.  Long term, we want to figure out how to update
  -- PowerPC safely.
  }

-- | Compose a list of instrumentation functions into a single
-- function suitable for use as an argument to 'redirect'
--
-- The instrumentors are applied in order; that order must be
-- carefully chosen, as the instrumentors are not isolated from each
-- other.
compose :: (Monad m)
        => [B.SymbolicBlock arch -> m (Maybe [B.TaggedInstruction arch (B.InstructionAnnotation arch)])]
        -> (B.SymbolicBlock arch -> m (Maybe [B.TaggedInstruction arch (B.InstructionAnnotation arch)]))
compose funcs = go funcs
  where
    go [] b = return $! Just (B.basicBlockInstructions b)
    go (f:fs) b = do
      mb_is <- f b
      case mb_is of
        Just is -> go fs b { B.basicBlockInstructions = is }
        Nothing -> go fs b

-- | An identity rewriter (i.e., a rewriter that makes no changes, but forces
-- everything to be redirected).
identity :: env arch binFmt -> b arch -> rewriterState arch -> B.SymbolicBlock arch -> RW.RewriteM arch (Maybe [B.TaggedInstruction arch (B.InstructionAnnotation arch)])
identity _ _ _ sb = return $! Just (B.basicBlockInstructions sb)

-- | A basic block rewriter that leaves a block untouched, preventing the
-- rewriter from trying to relocate it.
nop :: env arch binFmt -> b arch -> rewriterState arch -> B.SymbolicBlock arch -> RW.RewriteM arch (Maybe [B.TaggedInstruction arch (B.InstructionAnnotation arch)])
nop _ _ _ _ = return Nothing
