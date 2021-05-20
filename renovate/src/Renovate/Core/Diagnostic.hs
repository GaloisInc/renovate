{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
module Renovate.Core.Diagnostic (
    Diagnostic(..)
  , ELFDiagnostic(..)
  , RedirectionDiagnostic(..)
  , RecoveryDiagnostic(..)
  ) where

import qualified Data.ElfEdit as Elf
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Refinement as MR
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text as T
import           Data.Word ( Word16, Word64 )
import qualified Prettyprinter as PP

import qualified Renovate.Core.Address as RCA
import qualified Renovate.Core.BasicBlock as RCB
import qualified Renovate.ISA as ISA

data ELFDiagnostic where
  -- | Errors that can arise while parsing an ELF file
  --
  -- We can't stream these with more granularity because the elf library
  -- provides them all at once (and it is pure)
  ELFParseErrors :: [Elf.ElfParseError] -> ELFDiagnostic
  -- | Report the index assigned to a section that was modified during the ELF
  -- rewriting phase
  ELFSectionNewIndex :: String -> Word16 -> ELFDiagnostic
  -- | Report the size of the original .text section
  ELFOriginalTextSize :: Word -> ELFDiagnostic
  -- | Report the size of the additional program text required
  ELFNewTextSize :: Word -> ELFDiagnostic
  -- | Report that the ELF rewriter has overwritten the named region with the given number of bytes
  ELFOverwroteRegion :: String -> Word64 -> ELFDiagnostic

instance PP.Pretty ELFDiagnostic where
  pretty d =
    case d of
      ELFParseErrors errs ->
        PP.vcat ( PP.pretty "Errors parsing ELF file:"
                : fmap (PP.indent 2 . PP.viaShow) errs
                )
      ELFSectionNewIndex secName idx ->
        PP.hsep [ PP.pretty "Placing section"
                , PP.pretty secName
                , PP.pretty "at index"
                , PP.pretty idx
                ]
      ELFOriginalTextSize byteCount ->
        PP.pretty "Original .text section size was " <> PP.pretty byteCount <> PP.pretty " bytes"
      ELFNewTextSize byteCount ->
        PP.pretty "Additional .text section size is " <> PP.pretty byteCount <> PP.pretty " bytes"
      ELFOverwroteRegion region byteCount ->
        PP.hsep [ PP.pretty "Overwrote ELF region"
                , PP.pretty region
                , PP.pretty "with"
                , PP.pretty byteCount
                , PP.pretty "bytes"
                ]

data RedirectionDiagnostic where
  -- | The original block was too small to fit a redirecting jump
  InsufficientBlockSize :: (MC.MemWidth (MC.ArchAddrWidth arch))
                        => ISA.ISA arch -- ^ The ISA, required to interpret the blocks
                        -> Word -- ^ The required jump size
                        -> RCB.ConcreteBlock arch -- ^ The original block
                        -> RCB.ConcretizedBlock arch -- ^ The rewritten block
                        -> RedirectionDiagnostic

  -- | During the layout phase, reused this many bytes from an existing block
  ReusedBytes :: (MC.MemWidth (MC.ArchAddrWidth arch)) => RCA.ConcreteAddress arch -> Word -> RedirectionDiagnostic

  -- | This basic block could not be redirected (so the rewriter was never called on it)
  --
  -- The terminator field is redundant but a bit annoying to recompute, so it is
  -- included here
  CannotRedirect :: (MC.MemWidth (MC.ArchAddrWidth arch))
                 => ISA.ISA arch -- ^ The ISA (to interpret the block)
                 -> RCB.ConcreteBlock arch -- ^ The basic block that could not be redirected
                 -> Some (ISA.JumpType arch) -- ^ The terminator of the block
                 -> Bool -- ^ True if the block is incomplete (either directly or transitively)
                 -> Bool -- ^ True if the block is disjoint from all other blocks
                 -> RedirectionDiagnostic

instance PP.Pretty RedirectionDiagnostic where
  pretty d =
    case d of
      ReusedBytes addr byteCount ->
        PP.hsep [ PP.pretty "Reused"
                , PP.pretty byteCount
                , PP.pretty "bytes at address"
                , PP.pretty addr
                ]
      InsufficientBlockSize _isa reqJumpSize origBlock _rewrittenBlock ->
        PP.hsep [ PP.pretty "The basic block at address"
                , PP.pretty (RCB.concreteBlockAddress origBlock)
                , PP.pretty "is too small to accommodate a redirecting jump, which requires"
                , PP.pretty reqJumpSize
                , PP.pretty "bytes"
                ]
      CannotRedirect _isa block (Some jt) isIncomplete isDisjoint ->
        let reason = if | isIncomplete -> PP.pretty "the block is incomplete"
                        | not isDisjoint -> PP.pretty "the block overlaps another"
                        | ISA.IndirectJump {} <- jt -> PP.pretty "the block ends in an indirect jump"
                        | ISA.NotInstrumentable {} <- jt -> PP.pretty "the block ends in a non-instrumentable terminator"
                        | otherwise -> PP.pretty "the block is not instrumentable"
        in PP.hsep [ PP.pretty "The basic block at address"
                   , PP.pretty (RCB.concreteBlockAddress block)
                   , PP.pretty "cannot be redirected, so the rewriter was never called on it:"
                   , reason
                   ]

data RecoveryDiagnostic where
  RefinementLog :: (MC.MemWidth (MC.ArchAddrWidth arch)) => MR.RefinementLog arch -> RecoveryDiagnostic
  ClassificationFailure :: (MC.MemWidth w)
                        => MC.MemSegmentOff w
                        -> [String]
                        -> RecoveryDiagnostic
  TranslationError :: (MC.MemWidth w)
                   => MC.MemSegmentOff w
                   -> T.Text
                   -> RecoveryDiagnostic

instance PP.Pretty RecoveryDiagnostic where
  pretty d =
    case d of
      RefinementLog rl -> PP.pretty rl
      ClassificationFailure addr msgs ->
        PP.vcat ( PP.hcat [ PP.pretty "Code discovery classification failure at address "
                          , PP.viaShow addr
                          ]
                : fmap (PP.indent 2 . PP.pretty) msgs
                )
      TranslationError addr msg ->
        PP.hcat [ PP.pretty "Code discovery translation error at address "
                , PP.viaShow addr
                , PP.pretty ": "
                , PP.pretty msg
                ]

-- | The types of diagnostics that can be generated by renovate
--
-- This is a mix of (recoverable) errors, warnings, diagnostic information, and
-- metrics. Diagnostics will be streamed out of renovate core by writing them to
-- a lumberjack logging stream (via 'Lumberjack.LogAction')
--
-- The @l@ type provides an extensible way for library clients to generate their
-- own diagnostics during rewriting.
data Diagnostic l where
  -- | Diagnostics related to ELF handling
  ELFDiagnostic :: ELFDiagnostic -> Diagnostic l
  -- | Diagnostics related to code discovery
  RecoveryDiagnostic :: RecoveryDiagnostic -> Diagnostic l
  -- | Diagnostics that arise during the basic block redirection phase
  RedirectionDiagnostic :: RedirectionDiagnostic -> Diagnostic l

  -- | A diagnostic describing a change made to a single basic block
  RewriteSite :: (MC.MemWidth (MC.ArchAddrWidth arch))
              => RCB.SymbolicInfo arch -- ^ The address of the block modified
              -> Word                  -- ^ The instruction index into the block that was modified
              -> String                -- ^ A description of the change made
              -> Diagnostic l

  -- | An extension diagnostic from the client
  --
  -- If a client has no need for extensions, @l@ can be instantiated as
  -- 'Data.Void.Void'
  Extension :: l -> Diagnostic l

instance (PP.Pretty l) => PP.Pretty (Diagnostic l) where
  pretty diag =
    case diag of
      Extension l -> PP.pretty l
      RewriteSite symInfo insnIdx desc ->
        PP.hcat [ PP.pretty "Rewrite applied in block "
                , PP.pretty (RCB.concreteAddress symInfo)
                , PP.pretty " at instruction index "
                , PP.pretty insnIdx
                , PP.pretty ": "
                , PP.pretty desc
                ]
      ELFDiagnostic d -> PP.pretty d
      RecoveryDiagnostic d -> PP.pretty d
      RedirectionDiagnostic d -> PP.pretty d
