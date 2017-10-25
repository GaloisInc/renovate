{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- | Internal helpers for the ELF rewriting interface
module Renovate.Config (
  RenovateConfig(..),
  SomeConfig(..),
  compose,
  identity,
  nop
  ) where

import           GHC.TypeLits ( KnownNat )

import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import           Data.Typeable ( Typeable )

import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Macaw.Architecture.Info as MM
import qualified Data.Macaw.Memory as MM

import qualified Renovate.BasicBlock as B
import qualified Renovate.ISA as ISA
import qualified Renovate.Rewrite as RW
import qualified Renovate.Recovery as R

data SomeConfig b = forall i a w arch
                  . (ISA.InstructionConstraints i a,
                     R.ArchBits arch w,
                     KnownNat w, Typeable w)
                  => SomeConfig (NR.NatRepr w) (RenovateConfig i a w arch b)

-- | The configuration required for a run of the binary rewriter.
--
-- The binary rewriter is agnostic to the container format of the binary (e.g.,
-- ELF, COFF, Mach-O).  This configuration contains all of the information
-- necessary to analyze and rewrite a binary.
data RenovateConfig i a w arch b =
  RenovateConfig { rcISA           :: ISA.ISA i a w
                 , rcArchInfo      :: MM.ArchitectureInfo arch
                 , rcAssembler     :: forall m . (C.MonadThrow m) => i () -> m B.ByteString
                 , rcDisassembler :: forall m . (C.MonadThrow m) => B.ByteString -> m (Int, i ())
                 , rcAnalysis      :: ISA.ISA i a w -> MM.Memory w -> R.BlockInfo i w arch -> b
                 , rcRewriter      :: b -> ISA.ISA i a w -> MM.Memory w -> B.SymbolicBlock i a w -> RW.RewriteM i w (Maybe [B.TaggedInstruction i a])
                 }

-- | Compose a list of instrumentation functions into a single
-- function suitable for use as an argument to 'redirect'
--
-- The instrumentors are applied in order; that order must be
-- carefully chosen, as the instrumentors are not isolated from each
-- other.
compose :: (Monad m)
        => [B.SymbolicBlock i a w -> m (Maybe [B.TaggedInstruction i a])]
        -> (B.SymbolicBlock i a w -> m (Maybe [B.TaggedInstruction i a]))
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
identity :: (Monad m) => b -> B.SymbolicBlock i a w -> m (Maybe [B.TaggedInstruction i a])
identity _ sb = return $! Just (B.basicBlockInstructions sb)

nop :: Monad m => b -> B.SymbolicBlock i a w -> m (Maybe [B.TaggedInstruction i a])
nop _ _ = return Nothing
