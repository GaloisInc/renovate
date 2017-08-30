{-# LANGUAGE RankNTypes #-}
-- | Internal helpers for the ELF rewriting interface
module Renovate.ELF.Internal (
  RewriterConfig(..)
  ) where

import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B

import qualified Data.Macaw.Architecture.Info as MM

import qualified Renovate.Redirect as SFE
import qualified Renovate.ISA as SFE

import qualified Renovate.Instrument as I

-- | The configuration required for a run of the ELF rewriter.
data RewriterConfig i a w arch =
  RewriterConfig { rcISA :: SFE.ISA i a w
                 , rcArchInfo :: MM.ArchitectureInfo arch
                 , rcAssembler :: forall m . (C.MonadThrow m) => i () -> m B.ByteString
                 , rcDisassembler :: forall m . (C.MonadThrow m) => B.ByteString -> m [i ()]
                 , rcDisassembler1 :: forall m . (C.MonadThrow m) => B.ByteString -> m (Int, i ())
                 , rcInstrumentor :: SFE.SymbolicBlock i a w -> I.Instrument i w [SFE.TaggedInstruction i a]
                 }
