{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Renovate.BinaryFormat.ELF.Rewriter (
  ElfRewriter,
  runElfRewriter,
  RewriterInfo(..),
  emptyRewriterInfo,
  SomeBlocks(..),
  -- * Lenses
  riInitialBytes,
  riSmallBlockCount,
  riReusedByteCount,
  riUnrelocatableTerm,
  riEntryPointAddress,
  riSectionBaseAddress,
  riInstrumentationSites,
  riSegmentVirtualAddress,
  riOverwrittenRegions,
  riAppendedSegments,
  riRecoveredBlocks,
  riOriginalTextSize,
  riNewTextSize,
  riIncompleteBlocks,
  riRedirectionDiagnostics,
  riBlockRecoveryDiagnostics,
  riBlockMapping
  ) where

import           GHC.Generics ( Generic )

import           Control.Applicative
import qualified Control.Lens as L
import qualified Control.Monad.Catch.Pure as P
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.State.Strict as S
import qualified Data.ByteString as B
import qualified Data.Generics.Product as GL
import           Data.Word ( Word16, Word64 )

import           Prelude

import qualified Data.ElfEdit as E
import qualified Data.Macaw.CFG as MM

import qualified Renovate.Address as RA
import qualified Renovate.BasicBlock as B
import qualified Renovate.Diagnostic as RD
import qualified Renovate.ISA as ISA
import qualified Renovate.Rewrite as RW

-- | Statistics gathered and diagnostics generated during the
-- rewriting phase.
data RewriterInfo arch =
  RewriterInfo { _riSegmentVirtualAddress :: Maybe Word64
               , _riOverwrittenRegions :: [(String, Word64)]
               -- ^ The name of a data region and its length (which is
               -- the number of zero bytes that that replaced it)
               , _riAppendedSegments :: [(E.ElfSegmentType, Word16, Word64, Word64)]
               -- ^ The type of the segment, the index of the segment,
               -- the aligned offset at which it will be placed, the
               -- amount of padding required.
               , _riEntryPointAddress :: Maybe Word64
               , _riSectionBaseAddress :: Maybe Word64
               , _riInitialBytes :: Maybe B.ByteString
               , _riBlockRecoveryDiagnostics :: [RD.Diagnostic]
               , _riRedirectionDiagnostics :: [RD.Diagnostic]
               , _riRecoveredBlocks :: Maybe SomeBlocks
               , _riInstrumentationSites :: [RW.RewriteSite arch]
               , _riELF :: E.Elf (MM.ArchAddrWidth arch)
               , _riSmallBlockCount :: Int
               -- ^ The number of blocks that were too small to rewrite
               , _riReusedByteCount :: Int
               -- ^ The number of bytes re-used in the base program by the compact layout strategy
               , _riUnrelocatableTerm :: Int
               -- ^ The number of blocks that could not be relocated because
               -- they end with an IP-relative jump
               , _riOriginalTextSize :: Int
               -- ^ The number of bytes in the original text section
               , _riNewTextSize :: Int
               -- ^ The number of bytes allocated in the new text section
               , _riIncompleteBlocks :: Int
               -- ^ The number of blocks in incomplete functions
               , _riBlockMapping :: [(RA.ConcreteAddress arch, RA.ConcreteAddress arch)]
               -- ^ A mapping of original block addresses to rewritten block addresses
               }
  deriving (Generic)

data SomeBlocks = forall arch
                . (B.InstructionConstraints arch)
                => SomeBlocks (ISA.ISA arch) [B.ConcreteBlock arch]

newtype ElfRewriter arch a = ElfRewriter { unElfRewrite :: S.StateT (RewriterInfo arch) IO a }
                          deriving (Monad,
                                    Functor,
                                    Applicative,
                                    IO.MonadIO,
                                    P.MonadThrow,
                                    S.MonadState (RewriterInfo arch))

runElfRewriter :: E.Elf (MM.ArchAddrWidth arch)
               -> ElfRewriter arch a
               -> IO (a, RewriterInfo arch)
runElfRewriter e a =
  S.runStateT (unElfRewrite a) (emptyRewriterInfo e)

emptyRewriterInfo :: E.Elf (MM.ArchAddrWidth arch) -> RewriterInfo arch
emptyRewriterInfo e = RewriterInfo { _riSegmentVirtualAddress    = Nothing
                                   , _riOverwrittenRegions       = []
                                   , _riAppendedSegments         = []
                                   , _riEntryPointAddress        = Nothing
                                   , _riSectionBaseAddress       = Nothing
                                   , _riInitialBytes             = Nothing
                                   , _riBlockRecoveryDiagnostics = []
                                   , _riRedirectionDiagnostics   = []
                                   , _riRecoveredBlocks          = Nothing
                                   , _riInstrumentationSites     = []
                                   , _riELF                      = e
                                   , _riSmallBlockCount          = 0
                                   , _riReusedByteCount          = 0
                                   , _riUnrelocatableTerm        = 0
                                   , _riOriginalTextSize         = 0
                                   , _riIncompleteBlocks         = 0
                                   , _riNewTextSize              = 0
                                   , _riBlockMapping             = []
                                   }
riOriginalTextSize :: L.Simple L.Lens (RewriterInfo arch) Int
riOriginalTextSize = GL.field @"_riOriginalTextSize"

riNewTextSize :: L.Simple L.Lens (RewriterInfo arch) Int
riNewTextSize = GL.field @"_riNewTextSize"

riIncompleteBlocks :: L.Simple L.Lens (RewriterInfo arch) Int
riIncompleteBlocks = GL.field @"_riIncompleteBlocks"

riSegmentVirtualAddress :: L.Simple L.Lens (RewriterInfo arch) (Maybe Word64)
riSegmentVirtualAddress = GL.field @"_riSegmentVirtualAddress"

riOverwrittenRegions :: L.Simple L.Lens (RewriterInfo arch) [(String, Word64)]
riOverwrittenRegions = GL.field @"_riOverwrittenRegions"

riAppendedSegments :: L.Simple L.Lens (RewriterInfo arch) [(E.ElfSegmentType, Word16, Word64, Word64)]
riAppendedSegments = GL.field @"_riAppendedSegments"

riEntryPointAddress :: L.Simple L.Lens (RewriterInfo arch) (Maybe Word64)
riEntryPointAddress = GL.field @"_riEntryPointAddress"

riSectionBaseAddress :: L.Simple L.Lens (RewriterInfo arch) (Maybe Word64)
riSectionBaseAddress = GL.field @"_riSectionBaseAddress"

riInitialBytes :: L.Simple L.Lens (RewriterInfo arch) (Maybe B.ByteString)
riInitialBytes = GL.field @"_riInitialBytes"

riBlockRecoveryDiagnostics :: L.Simple L.Lens (RewriterInfo arch) [RD.Diagnostic]
riBlockRecoveryDiagnostics = GL.field @"_riBlockRecoveryDiagnostics"

riRedirectionDiagnostics :: L.Simple L.Lens (RewriterInfo arch) [RD.Diagnostic]
riRedirectionDiagnostics = GL.field @"_riRedirectionDiagnostics"

riRecoveredBlocks :: L.Simple L.Lens (RewriterInfo arch) (Maybe SomeBlocks)
riRecoveredBlocks = GL.field @"_riRecoveredBlocks"

riInstrumentationSites :: L.Simple L.Lens (RewriterInfo arch) [RW.RewriteSite arch]
riInstrumentationSites = GL.field @"_riInstrumentationSites"

riSmallBlockCount :: L.Simple L.Lens (RewriterInfo arch) Int
riSmallBlockCount = GL.field @"_riSmallBlockCount"

riReusedByteCount :: L.Simple L.Lens (RewriterInfo arch) Int
riReusedByteCount = GL.field @"_riReusedByteCount"

riUnrelocatableTerm :: L.Simple L.Lens (RewriterInfo arch) Int
riUnrelocatableTerm = GL.field @"_riUnrelocatableTerm"

riBlockMapping :: L.Simple L.Lens (RewriterInfo arch) [(RA.ConcreteAddress arch, RA.ConcreteAddress arch)]
riBlockMapping = GL.field @"_riBlockMapping"

