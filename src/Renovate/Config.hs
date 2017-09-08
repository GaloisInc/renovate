{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds  #-}
-- | Internal helpers for the ELF rewriting interface
module Renovate.Config (
  RenovateConfig(..),
  Rewriter(..),
  compose,
  identity
  ) where

import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B

import qualified Data.Macaw.Architecture.Info as MM

import qualified Renovate.Arch.X86_64.Internal as X86

import qualified Renovate.BasicBlock as B
import qualified Renovate.ISA as ISA
import qualified Renovate.Rewrite as RW

-- | The configuration required for a run of the ELF rewriter.
data RenovateConfig i a w arch =
  RenovateConfig { rcISA           :: ISA.ISA i a w
                 , rcArchInfo      :: MM.ArchitectureInfo arch
                 , rcAssembler     :: forall m . (C.MonadThrow m) => i () -> m B.ByteString
                 , rcDisassembler  :: forall m . (C.MonadThrow m) => B.ByteString -> m [i ()]
                 , rcDisassembler1 :: forall m . (C.MonadThrow m) => B.ByteString -> m (Int, i ())
                 , rcInstrumentor  :: B.SymbolicBlock i a w -> RW.RewriteM i w [B.TaggedInstruction i a]
                 }

data Rewriter = Rewriter
  { iX86_64 :: B.SymbolicBlock X86.Instruction (X86.TargetAddress 64) 64
            -> RW.RewriteM X86.Instruction 64 [B.TaggedInstruction X86.Instruction (X86.TargetAddress 64)]
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
identity :: (Monad m) => B.SymbolicBlock i a w -> m [B.TaggedInstruction i a]
identity sb = return (B.basicBlockInstructions sb)
