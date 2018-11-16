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
  Analyze,
  Rewrite,
  AnalyzeEnv(..),
  RenovateConfig(..),
  SomeConfig(..),
  TrivialConfigConstraint,
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
-- * The constraint @c@ supports exposing information about the hidden params, or
--   bundling up other functionality via a type class.
--
-- * The result type @b@ is the type of results of the pre-rewriting analysis
--   pass.  It is parameterized by the architecture of the analysis in such a
--   way that there can be a list of 'SomeConfig' while still containing
--   architecture-parameterized data (where the architecture is hidden by the
--   existential).
data SomeConfig c (b :: * -> *) = forall arch binFmt
                  . (B.InstructionConstraints arch,
                     R.ArchBits arch,
                     MBL.BinaryLoader arch binFmt,
                     c arch b)
                  => SomeConfig (NR.NatRepr (MM.ArchAddrWidth arch)) (MBL.BinaryRepr binFmt) (RenovateConfig arch binFmt b)

-- | A trivial constraint for use with 'SomeConfig'.
class TrivialConfigConstraint arch b
instance TrivialConfigConstraint arch b

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
-- * @b@ (which is applied to @arch@) the type of analysis results produced by the analysis and passed to the rewriter
data RenovateConfig arch binFmt (b :: * -> *) = RenovateConfig
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
  , rcAnalysis      :: Analyze arch binFmt b
  -- ^ An analysis to run over the code discovered by macaw, generating a summary of type @b@
  , rcRewriter      :: Rewrite arch binFmt b
  -- ^ A rewriting pass to run over each basic block
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

-- | The type of 'rcAnalysis'.
--
-- The motivation for analysis being in IO is to allow SFE to call out
-- to external tools, such as ILP solvers.
type Analyze arch binFmt b = AnalyzeEnv arch -> MBL.LoadedBinary arch binFmt -> IO (b arch)
-- | The type of 'rcRewriter'.
type Rewrite arch binFmt b = b arch
                          -> MBL.LoadedBinary arch binFmt
                          -> B.SymbolicBlock arch
                          -> RW.RewriteM arch (Maybe [B.TaggedInstruction arch (B.InstructionAnnotation arch)])

-- | Environment for 'rcAnalysis'.
data AnalyzeEnv arch =
  AnalyzeEnv { aeRewriteEnv :: RW.RewriteEnv arch
             , aeSymbolicBlockMap :: Map (RA.ConcreteAddress arch) (B.SymbolicBlock arch)
               -- ^ Mapping from concrete addresses to corresponding
               -- symbolic blocks.
             , aeRunRewriteM :: forall a. RW.RewriteM arch a -> (a, RW.RewriteInfo arch)
               -- ^ Runner for 'RW.RewriteM' computations, provided
               -- for simulating transforms at analysis time.
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
identity :: Rewrite arch binFmt b
identity _ _ sb = return $! Just (B.basicBlockInstructions sb)

-- | A basic block rewriter that leaves a block untouched, preventing the
-- rewriter from trying to relocate it.
nop :: Rewrite arch binFmt b
nop _ _ _ = return Nothing
