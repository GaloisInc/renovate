{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ExistentialQuantification #-}
-- | Internal helpers for the ELF rewriting interface
module Renovate.Config (
  RenovateConfig(..),
  Rewriter(..),
  Analysis(..),
  compose,
  identity
  ) where

import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B

import qualified Data.Macaw.Architecture.Info as MM
import qualified Data.Macaw.Memory as MM

import qualified Renovate.Arch.X86_64.Internal as X86

import qualified Renovate.BasicBlock as B
import qualified Renovate.ISA as ISA
import qualified Renovate.Rewrite as RW
import qualified Renovate.Recovery as R

-- | The configuration required for a run of the binary rewriter.
--
-- The binary rewriter is agnostic to the container format of the binary (e.g.,
-- ELF, COFF, Mach-O).  This configuration contains all of the information
-- necessary to analyze and rewrite a binary.
data RenovateConfig i a w arch b =
  RenovateConfig { rcISA           :: ISA.ISA i a w
                 , rcArchInfo      :: MM.ArchitectureInfo arch
                 , rcAssembler     :: forall m . (C.MonadThrow m) => i () -> m B.ByteString
                 , rcDisassembler  :: forall m . (C.MonadThrow m) => B.ByteString -> m [i ()]
                 , rcDisassembler1 :: forall m . (C.MonadThrow m) => B.ByteString -> m (Int, i ())
                 , rcAnalysis      :: ISA.ISA i a w -> MM.Memory w -> R.BlockInfo i w -> b
                 , rcRewriter      :: b -> B.SymbolicBlock i a w -> RW.RewriteM i w [B.TaggedInstruction i a]
                 }

-- | The rewriting action to take
--
-- Callers must specify a rewriting action for each platform they wish to
-- support.  Binary rewriting is architecture-specific.
data Rewriter a = Rewriter
  { iX86_64 :: a
            -> B.SymbolicBlock X86.Instruction (X86.TargetAddress 64) 64
            -> RW.RewriteM X86.Instruction 64 [B.TaggedInstruction X86.Instruction (X86.TargetAddress 64)]
    -- ^ A rewriter suitable for the x86_64 architecture
  }

data Analysis a = Analysis
  { aX86_64 :: ISA.ISA X86.Instruction (X86.TargetAddress 64) 64 -> MM.Memory 64 -> R.BlockInfo X86.Instruction 64 -> a
  }

-- | Compose a list of instrumentation functions into a single
-- function suitable for use as an argument to 'redirect'
--
-- The instrumentors are applied in order; that order must be
-- carefully chosen, as the instrumentors are not isolated from each
-- other.
compose :: (Monad m)
        => [B.SymbolicBlock i a w -> m [B.TaggedInstruction i a]]
        -> (B.SymbolicBlock i a w -> m [B.TaggedInstruction i a])
compose funcs = go funcs
  where
    go [] b = return $ B.basicBlockInstructions b
    go (f:fs) b = do
      is <- f b
      go fs b { B.basicBlockInstructions = is }

-- | An identity rewriter (i.e., a rewriter that makes no changes, but forces
-- everything to be redirected).
identity :: (Monad m) => b -> B.SymbolicBlock i a w -> m [B.TaggedInstruction i a]
identity _ sb = return (B.basicBlockInstructions sb)
