{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | An interface for manipulating ELF files
--
-- It provides a convenient interface for loading an ELF file,
-- applying a set of transformations to the text section, and
-- reassembling a working ELF file with the results.
--
-- It works by redirecting all of the code in the original text
-- section to rewritten versions in a new section.  It handles
-- creating the new section and fixing up all of the ELF metadata.
module Renovate.BinaryFormat.ELF (
  withElfConfig,
  withMemory,
  rewriteElf,
  analyzeElf,
  RewriterInfo,
  RewriterEnv,
  SomeConcreteBlocks(..),
  SomeConcretizedBlocks(..),
  RE.SectionInfo(..),
  reSegmentMaximumSize,
  reSegmentVirtualAddress,
  -- * Lenses
  riInitialBytes,
  riSmallBlockCount,
  riReusedByteCount,
  riUnrelocatableTerm,
  riEntryPointAddress,
  riSectionBaseAddress,
  riInstrumentationSites,
  riLogMsgs,
  riOverwrittenRegions,
  riAppendedSegments,
  riRecoveredBlocks,
  riOriginalTextSize,
  riNewTextSize,
  riIncompleteBlocks,
  riRedirectionDiagnostics,
  riBlockRecoveryDiagnostics,
  riDiscoveredBlocks,
  riInstrumentedBytes,
  riBlockMapping,
  riBackwardBlockMapping,
  riOutputBlocks,
  riRewritePairs,
  riFunctionBlocks,
  riSections,
  riTranslationErrors,
  riClassifyFailures,
  ) where

import           Control.Applicative
import           Control.Arrow ( second )
import qualified Control.Lens as L
import           Control.Lens ( (^.) )
import           Control.Monad ( guard, when )
import qualified Control.Monad.Catch as C
import qualified Control.Monad.Catch.Pure as P
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as S
import           Data.Bits ( Bits, (.|.) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import           Data.Maybe ( fromMaybe, maybeToList, listToMaybe, mapMaybe, isJust )
import           Data.Monoid
import qualified Data.Ord as O
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Sequence as Seq
import           Data.Typeable ( Typeable )
import qualified Data.Vector as V
import           Data.Word ( Word16 )
import           GHC.TypeLits
import           Text.Printf ( printf )

import           Prelude

import qualified Data.ElfEdit as E
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.Memory.ElfLoader as MM
import qualified Data.Macaw.Symbolic as MS
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.NatRepr as NR
import qualified Lang.Crucible.FunctionHandle as C

import qualified Renovate.Address as RA
import qualified Renovate.Analysis.FunctionRecovery as FR
import qualified Renovate.Arch as Arch
import qualified Renovate.BasicBlock as B
import qualified Renovate.BasicBlock.Assemble as BA
import           Renovate.BinaryFormat.ELF.BSS ( expandBSS )
import           Renovate.BinaryFormat.ELF.Common
import           Renovate.BinaryFormat.ELF.Internal
import           Renovate.BinaryFormat.ELF.Rewriter as Rewriter
import           Renovate.Config
import qualified Renovate.Diagnostic as RD
import qualified Renovate.Metrics as RM
import qualified Renovate.Recovery as R
import qualified Renovate.Redirect as RE
import qualified Renovate.Redirect.LayoutBlocks.Types as RT
import qualified Renovate.Redirect.Symbolize as RS
import qualified Renovate.Rewrite as RW

-- | For a given 'E.Elf' file, select the provided configuration that applies to it
--
-- This function examines the metadata of the given 'E.Elf' to determine which
-- (if any) of the given configurations are applicable.  If there is an
-- applicable configuration, the continuation is applied to the configuration
-- and 'E.Elf' file.
--
-- Note that the continuation is also provided with an 'MBL.LoadedBinary'.  That
-- type is only accessible in the callback, as it is indexed by the
-- architecture (which is not known until a configuration is selected).
--
-- If no configurations apply, the continuation is not invoked and an
-- 'UnsupportedArchitecture' exception is thrown.
--
-- Supported architectures are listed in the Renovate.Arch module hierarchy.
withElfConfig :: (C.MonadThrow m)
              => E.SomeElf E.Elf
              -> [(Arch.Architecture, SomeConfig callbacks b)]
              -> (forall arch . (MS.SymArchConstraints arch,
                                  Typeable arch,
                                  16 <= MM.ArchAddrWidth arch,
                                  MBL.BinaryLoader arch (E.Elf (MM.ArchAddrWidth arch)),
                                  E.ElfWidthConstraints (MM.ArchAddrWidth arch),
                                  B.InstructionConstraints arch)
                                   => RenovateConfig arch (E.Elf (MM.ArchAddrWidth arch)) callbacks b
                                   -> E.Elf (MM.ArchAddrWidth arch)
                                   -> MBL.LoadedBinary arch (E.Elf (MM.ArchAddrWidth arch))
                                   -> m t)
              -> m t
withElfConfig e0 configs k = do
  case (e0, withElf e0 E.elfMachine) of
    (E.Elf32 e, E.EM_PPC) ->
      case lookup Arch.PPC32 configs of
        Nothing -> C.throwM (UnsupportedArchitecture E.EM_PPC)
        Just (SomeConfig nr binRep cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @32)
          , Just PC.Refl <- PC.testEquality binRep MBL.Elf32Repr -> do
              MBL.loadBinary loadOpts e >>= k cfg e
          | otherwise -> error ("Invalid NatRepr for PPC32: " ++ show nr)
    (E.Elf32 e, E.EM_ARM) ->
      case lookup Arch.ARM configs of
        Nothing -> C.throwM (UnsupportedArchitecture E.EM_ARM)
        Just (SomeConfig nr binRep cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @32)
          , Just PC.Refl <- PC.testEquality binRep MBL.Elf32Repr -> do
              MBL.loadBinary loadOpts e >>= k cfg e
          | otherwise -> error ("Invalid NatRepr for AArch32: " ++ show nr)
    (E.Elf32 _, mach) -> C.throwM (UnsupportedArchitecture mach)
    (E.Elf64 e, E.EM_X86_64) ->
      case lookup Arch.X86_64 configs of
        Nothing -> C.throwM (UnsupportedArchitecture E.EM_X86_64)
        Just (SomeConfig nr binRep cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @64)
          , Just PC.Refl <- PC.testEquality binRep MBL.Elf64Repr ->
              MBL.loadBinary loadOpts e >>= k cfg e
          | otherwise -> error ("Invalid NatRepr for X86_64: " ++ show nr)
    (E.Elf64 e, E.EM_PPC64) ->
      case lookup Arch.PPC64 configs of
        Nothing -> C.throwM (UnsupportedArchitecture E.EM_PPC64)
        Just (SomeConfig nr binRep cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @64)
          , Just PC.Refl <- PC.testEquality binRep MBL.Elf64Repr ->
              MBL.loadBinary loadOpts e >>= k cfg e
          | otherwise -> error ("Invalid NatRepr for PPC64: " ++ show nr)
    (E.Elf64 _, mach) -> C.throwM (UnsupportedArchitecture mach)
  where
    loadOpts = MM.defaultLoadOptions { MM.loadOffset = Just 0 }

-- | Apply a rewriter to an ELF file using the chosen layout strategy.
--
-- The 'RE.LayoutStrategy' determines how rewritten basic blocks will be laid
-- out in the new binary file.  If the rewriter succeeds, it returns a new ELF
-- file and some metadata describing the changes made to the file.  Some of the
-- metadata is provided by rewriter passes in the 'RW.RewriteM' environment.
rewriteElf :: (B.InstructionConstraints arch,
               MBL.BinaryLoader arch binFmt,
               Typeable arch,
               E.ElfWidthConstraints (MM.ArchAddrWidth arch),
               16 <= MM.ArchAddrWidth arch,
               MS.SymArchConstraints arch)
           => RenovateConfig arch binFmt (AnalyzeAndRewrite lm) b
           -- ^ The configuration for the rewriter
           -> C.HandleAllocator
           -- ^ A handle allocator for allocating crucible function handles (used for lifting macaw->crucible)
           -> E.Elf (MM.ArchAddrWidth arch)
           -- ^ The ELF file to rewrite
           -> MBL.LoadedBinary arch binFmt
           -- ^ A representation of the contents of memory of the ELF file
           -- (including statically-allocated data)
           -> RE.LayoutStrategy
           -- ^ The layout strategy for blocks in the new binary
           -> IO (E.Elf (MM.ArchAddrWidth arch), b arch, RewriterInfo lm arch, RewriterEnv arch)
rewriteElf cfg hdlAlloc e loadedBinary strat = do
    (analysisResult, ri, env) <- runElfRewriter cfg e $ do
      -- FIXME: Use the symbol map from the loaded binary (which we still need to add)
      symmap <- withCurrentELF buildSymbolMap
      doRewrite cfg hdlAlloc loadedBinary symmap strat
    return (_riELF ri, analysisResult, ri, env)

-- | Run an analysis over an ELF file
--
-- Note that the configuration type is keyed by the 'AnalyzeOnly' tag, which
-- restricts the type of the analysis compared to the rewriting variant.
analyzeElf :: (B.InstructionConstraints arch,
               MBL.BinaryLoader arch binFmt,
               E.ElfWidthConstraints (MM.ArchAddrWidth arch),
               16 <= MM.ArchAddrWidth arch,
               MS.SymArchConstraints arch)
           => RenovateConfig arch binFmt AnalyzeOnly b
           -- ^ The configuration for the analysis
           -> C.HandleAllocator
           -> E.Elf (MM.ArchAddrWidth arch)
           -- ^ The ELF file to analyze
           -> MBL.LoadedBinary arch binFmt
           -- ^ A representation of the contents of memory of the ELF file
           -- (including statically-allocated data)
           -> IO (b arch, [RE.Diagnostic])
analyzeElf cfg hdlAlloc e loadedBinary = do
  (b, ri, _env) <- runElfRewriter cfg e $ do
    symmap <- withCurrentELF buildSymbolMap
    textSection <- withCurrentELF findTextSection
    let textRange = sectionAddressRange textSection
    withAnalysisEnv cfg hdlAlloc loadedBinary symmap textRange $ \env -> do
      IO.liftIO (aoAnalyze (rcAnalysis cfg) env)
  return (b, ri ^. riBlockRecoveryDiagnostics)

withElf :: E.SomeElf E.Elf -> (forall w . E.Elf w -> a) -> a
withElf e k =
  case e of
    E.Elf32 e32 -> k e32
    E.Elf64 e64 -> k e64

-- | Extract the 'MM.Memory' from an ELF file.
withMemory :: forall w m a arch
            . (C.MonadThrow m, MM.MemWidth w, Integral (E.ElfWordType w),
               w ~ MM.ArchAddrWidth arch,
               MBL.BinaryLoader arch (E.Elf w))
           => E.Elf w
           -> (MBL.LoadedBinary arch (E.Elf w) -> m a)
           -> m a
withMemory e k = do
  MBL.loadBinary loadOpts e >>= k
  where
    loadOpts = MM.defaultLoadOptions { MM.loadOffset = Just 0 }

-- | Call the given continuation with the current ELF file
--
-- The intention is that functions that simply *read* the current ELF file are
-- wrapped in this combinator as a marker that they are read-only.  Functions
-- that modify the current ELF file will be wrapped in 'modifyCurrentELF'.
withCurrentELF :: (w ~ MM.ArchAddrWidth arch) => (E.Elf w -> ElfRewriter lm arch a) -> ElfRewriter lm arch a
withCurrentELF k = do
  elf <- S.gets _riELF
  k elf

-- | A wrapper around functions that modify the current ELF file.
--
-- The modification function can return some extra data if desired.  The idea is that this is a signal that
-- a function mutates the ELF file.
--
-- This should be the only function that writes to the current ELF file
modifyCurrentELF :: (w ~ MM.ArchAddrWidth arch) => (E.Elf w -> ElfRewriter lm arch (a, E.Elf w)) -> ElfRewriter lm arch a
modifyCurrentELF k = do
  elf <- S.gets _riELF
  (res, elf') <- k elf
  S.modify' $ \s -> s { _riELF = elf' }
  return res

-- | Compute the starting and ending address (address range) of an ELF section
--
-- FIXME: It would be nice to bundle the return value up in an ADT instead of a pair
sectionAddressRange :: (MM.MemWidth (MM.ArchAddrWidth arch), Integral a)
                    => E.ElfSection a
                    -> (RE.ConcreteAddress arch, RE.ConcreteAddress arch)
sectionAddressRange sec = (textSectionStartAddr, textSectionEndAddr)
  where
    textSectionStartAddr = RA.concreteFromAbsolute (fromIntegral (E.elfSectionAddr sec))
    textSectionEndAddr = RA.addressAddOffset textSectionStartAddr (fromIntegral ((E.elfSectionSize sec)))

-- | The rewriter driver
--
-- This code handles pulling information out of the original ELF file, running a
-- rewriter over the original code, and re-constructing a new ELF file.  The
-- re-construction of the ELF file is subject to the following constraints:
--
-- * We cannot move the original code or data
-- * We require the PHDR table to be in the first loadable segment (the kernel
--   computes the mapped address of the PHDRs table under the assumption that
--   the first loadable segment contains the PHDR table; for TLS-enabled
--   binaries, this assumption results in an invalid pointer being read and used
--   by the startup code in glibc and musl)
-- * The address of the PHDR table must be >= its offset in the file (the kernel
--   doesn't check and does an invalid subtraction otherwise)
--
-- In addition to the above, a QEMU bug adds some finicky additional constraints
-- on the virtual address of the PHDR segment (documented in the comment on
-- 'choosePHDRSegmentAddress'). A separate QEMU bug requires that the last
-- segment in the program's virtual address space is writable.
--
-- The strategy we choose is to:
-- 1) Wipe out the original PHDRs table (along with all of the other dynamically-sized data)
-- 2) Append the new text segment (with the .extratext section) after all of the existing segments
-- 3) Append new copies of all of the dynamically-sized sections (including a new symbol table)
-- 4) Append a fresh PHDRs table at the very end *but* with segment index 0 (with the other segment
--    indexes suitably modified), which makes it the first loadable segment.
-- 5) Append an empty, writable, loadable segment at an address just after the new PHDRs.
--
-- TODO:
--  * Handle binaries that already contain a separate PHDR segment (do we need
--    to do anything special to remove it?)
--  * Fix handling of new data segments (reserve a fixed amount of space before the new text)
--  * More carefully analyze alignment requirements (the new PHDRs and new text
--    are currently page aligned - is that necessary?)
doRewrite :: (B.InstructionConstraints arch,
              MBL.BinaryLoader arch binFmt,
              Typeable arch,
              E.ElfWidthConstraints (MM.ArchAddrWidth arch),
              16 <= MM.ArchAddrWidth arch,
              MS.SymArchConstraints arch)
          => RenovateConfig arch binFmt (AnalyzeAndRewrite lm) b
          -> C.HandleAllocator
          -> MBL.LoadedBinary arch binFmt
          -> RE.SymbolMap arch
          -> RE.LayoutStrategy
          -> ElfRewriter lm arch (b arch)
doRewrite cfg hdlAlloc loadedBinary symmap strat = do
  -- We pull some information from the unmodified initial binary: the text
  -- section, the entry point(s), and original symbol table (if any).
  textSection <- withCurrentELF findTextSection
  mBaseSymtab <- withCurrentELF getBaseSymbolTable

  -- Remove (and pad out) the sections whose size could change if we
  -- modify the binary.  We'll re-add them later (see @appendHeaders@).
  --
  -- This modifies the underlying ELF file
  modifyCurrentELF padDynamicDataRegions

  -- The .bss is a special section in a binary that comes after the data
  -- section.  It holds zero-initialized data, and is not explicitly represented
  -- in the binary (aside from the entry in the section table).  The Linux
  -- kernel refuses to properly initialize .bss if there is a segment loaded
  -- before the .bss, but with a higher address.
  --
  -- To work around this, we expand the data section to cover the .bss region
  -- and explicitly initialize that data with zeros.
  modifyCurrentELF expandBSS

  -- Similarly, we need to find space to put new data.  Again, we can't just put
  -- new data directly after or before the actual .data section, as there are a
  -- number of things that come before and after it.  We can't put anything
  -- after .data, as .bss has to be the last section in the last segment.  This
  -- means that we have to find the segment containing .data and use the start
  -- address of the segment to start laying out new data (growing down, towards
  -- the loadable segment containing .text).

  -- Apply a rewriter to the binary
  --
  -- This computes the new contents of the .text section
  -- (overwrittenBytes) and the contents of the new code segment
  -- (instrumentedBytes), which will be placed at the address computed
  -- above.
  env <- R.ask
  let newTextAddr = Rewriter.reSegmentVirtualAddress env
  let newTextSize = Rewriter.reSegmentMaximumSize env
  let layoutAddr = RA.concreteFromAbsolute (fromIntegral newTextAddr)
      -- FIXME: This is wrong; it doesn't account for the required alignment we
      -- need.  That is a big challenge because it depends on how much code we
      -- generate.  Maybe we can do something with congruence where we waste up
      -- to a page of space to maintain alignment.
      dataAddr = RA.concreteFromAbsolute (fromIntegral (rcDataLayoutBase cfg))
      textSectionRange = sectionAddressRange textSection

  ( analysisResult
    , overwrittenBytes
    , instrumentedBytes
    , mNewData
    , newSyms
    , addrMap ) <- instrumentTextSection cfg hdlAlloc loadedBinary textSectionRange
                                       (E.elfSectionData textSection) strat layoutAddr dataAddr symmap

  let instrumentedByteCount = B.length instrumentedBytes
  when (fromIntegral instrumentedByteCount > newTextSize) . fail $
    "The rewritten binary needs " ++ show instrumentedByteCount ++ " bytes in the extratext section, but only " ++ show newTextSize ++ " are available."
  riOriginalTextSize L..= fromIntegral (B.length overwrittenBytes)
  riNewTextSize L..= instrumentedByteCount

  -- Since we know where we need to add new data and new text, we can just start
  -- with a fresh ELF file and start copying data over.  As soon as we find a
  -- segment containing .text, we know that we can copy everything over
  -- (recursively), but put our new section after that.
  --
  -- When we get to the point where we want to add our new data section, we'll
  -- have to add some padding before it to maintain alignment of everything
  -- coming after it.


  -- Allocate a new section for the extra bytes of program text we have generated.
  (newTextSecIdx, newTextSec) <- withCurrentELF (newTextSection newTextAddr instrumentedBytes)
  -- Wrap the new text section in a loadable segment
  newTextSegment <- withCurrentELF (newExecutableSegment newTextAddr (Seq.singleton (E.ElfDataSection newTextSec)))
  modifyCurrentELF (appendSegment newTextSegment)


  -- Update the symbol table (if there is one)
  --
  -- FIXME: If we aren't updating the symbol table (but there is one), just
  -- preserve the original one.  The current code just throws it away.
  case mBaseSymtab of
    Just baseSymtab
      | rcUpdateSymbolTable cfg -> do
          newSymtab <- withCurrentELF (buildNewSymbolTable (E.elfSectionIndex textSection) newTextSecIdx layoutAddr newSyms addrMap baseSymtab)
          let symbolTableAlignment = 8
          modifyCurrentELF (appendDataRegion (E.ElfDataSymtab newSymtab) symbolTableAlignment)
      | otherwise -> return ()
    _ -> return ()

  -- Append a fresh data segment (of size 1 page) with zero contents.
  --
  -- We need this to be at the highest address in the final binary (mostly to
  -- satisfy qemu, which sets the program brk to the highest address of any data
  -- segment).  NOTE: If we have concrete data contents, we need to overwrite
  -- the empty data later.  We must also make sure that the rewriter pass didn't
  -- ask for more space than we reserved.  If we find that to be a problem, we
  -- could probably just safely put this at the end of the binary after we
  -- figure out how much space we need.
  let dataPage = fromMaybe (B.pack (replicate 4096 0)) mNewData
  newDataSec <- withCurrentELF (newDataSection (fromIntegral (rcDataLayoutBase cfg)) dataPage)
  newDataSeg <- withCurrentELF (newDataSegment newDataSec)
  modifyCurrentELF (appendSegment newDataSeg)

  modifyCurrentELF appendHeaders
  -- Now overwrite the original code (in the .text segment) with the
  -- content computed by our transformation.
  modifyCurrentELF (overwriteTextSection overwrittenBytes)

  -- Add a PHDR segment which will hold the modified program header table.
  --
  -- We usually want this to be the first segment (have a segment index of 0)
  -- (see comment on "phdrSegment"), except that on ARM, if an EXIDX segment is
  -- present, that must come first.

  let isEXIDX seg = E.elfSegmentType seg == E.PT_ARM_EXIDX
  exidxs <- withCurrentELF (pure . filter isEXIDX . E.elfSegments)
  when (length exidxs > 1) $
    fail $ unwords $
      [ "Malformed input:"
      , "Found several ARM_EXIDX segments, at the following indices:"
      , show (map E.elfSegmentIndex exidxs)
      ]
  newSegmentIdx <-
    case E.elfSegmentIndex <$> listToMaybe exidxs of
      Nothing -> pure 0
      Just idx | idx == 0 -> pure 1
      Just idx ->
        fail $ unwords $
          [ "Malformed input: "
          , "Expected EXIDX segment to have index 0, but it had index"
          , show idx
          ]

  phdrSegmentAddress <- withCurrentELF (choosePHDRSegmentAddress Proxy)
  let newSegment = phdrSegment newSegmentIdx phdrSegmentAddress
      newSegmentCount = E.elfSegmentIndex newSegment + 1
  -- Increment all of the segment indexes (other than EXIDX) so that we can
  -- reserve the appropriate segment index for our fresh PHDR segment that we
  -- want at the beginning of the PHDR table (but not at the first offset)
  modifyCurrentELF (\e -> ((),) <$> E.traverseElfSegments (incrementSegmentNumber (not . isEXIDX) newSegmentCount) e)
  -- Note: We have to update the GNU Stack and GnuRelroRegion segment numbers
  -- independently, as they are handled specially in elf-edit.
  modifyCurrentELF (\e -> ((),) <$> fixOtherSegmentNumbers newSegmentCount e)
  -- Any PHDR segments that existed before have got wrong data or padding in
  -- them now, so they're no good to anybody. We'll keep them around so that
  -- other parts of the ELF don't shift around, but inform the audience that
  -- they're uninteresting.
  modifyCurrentELF (\e -> ((),) <$> E.traverseElfSegments nullifyPhdr e)
  modifyCurrentELF (appendSegment newSegment)

  -- Linux calculates the program break based on the virtual address and size of
  -- the LOAD segment with the highest virtual address, whereas QEMU calculates
  -- it based on the virtual address and size of the *writable* LOAD segment with
  -- the highest virtual address. Since our PHDR segment might have a high
  -- virtual address, we have to add a writable load segment after it, so our
  -- programs don't segfault inside QEMU.
  --
  -- https://bugs.launchpad.net/qemu/+bug/1886097

  phdrActualSize <-
    withCurrentELF $ \elf ->
      E.phdrMemSize <$>
        case filter ((== E.PT_PHDR) . E.phdrSegmentType) (E.allPhdrs (E.elfLayout elf)) of
          [seg] -> pure seg
          phdrs' ->
            fail $ unwords $
              [ "Internal error:"
              , "Wrong number of PT_PHDR segments:"
              , show phdrs'
              ]

  nextIdx <- withCurrentELF (pure . (+1) . nextSegmentIndex)
  modifyCurrentELF $ appendSegment $
    let alignedAddr =
          alignValue
            (E.elfSegmentVirtAddr newSegment + phdrActualSize)
            (fromIntegral pageAlignment)
    in E.ElfSegment
         { E.elfSegmentType = E.PT_LOAD
         , E.elfSegmentFlags = E.pf_w
         , E.elfSegmentIndex = nextIdx
         , E.elfSegmentVirtAddr = alignedAddr
         , E.elfSegmentPhysAddr = alignedAddr
         , E.elfSegmentAlign = 1
         , E.elfSegmentMemSize = E.ElfRelativeSize 0
         , E.elfSegmentData = Seq.singleton (E.ElfDataRaw "")
         }

  return analysisResult

nullifyPhdr :: Applicative f => E.ElfSegment w -> f (E.ElfSegment w)
nullifyPhdr s = pure $ case E.elfSegmentType s of
  E.PT_PHDR -> s { E.elfSegmentType = E.PT_NULL }
  _ -> s

-- | Choose the (base) virtual address to lay out the PHDR table at
--
-- When loading an ELF binary, the Linux kernel performs a calculation of the
-- virtual address of the program header table (@AT_PHDR@), and places the
-- result in the aux vector. This value is used by e.g. glibc's thread-local
-- storage initialization code. The address it calculates is
--
-- @AT_PHDR = (elf_ppnt->p_vaddr - elf_ppnt->p_offset) + exec->e_phoff@
--
-- where elf_ppnt is the first LOAD segment appearing in the program header
-- table (see linux/fs/binfmt_elf.c). QEMU's user-mode emulation also
-- calculates a value for @AT_PHDR@, but it does so differently. It uses
--
-- @AT_PHDR = min_i(phdr[i].p_vaddr - phdr[i].p_offset) + exec->e_phoff@
--
-- where @min_i@ is the minimum over indices @i@ of LOAD segments. We must
-- make these values coincide, so we have to ensure that the virtual address
-- of the new PHDR segment minus its offset must be the minimal difference
-- between any LOAD segment's address and offset.
--
-- See the relevant QEMU bug report for additional details:
-- https://bugs.launchpad.net/qemu/+bug/1885332
--
-- NOTE: We probably want to think more carefully about the alignment of this
-- address.  Currently, we page align it.
--
-- This function assumes:
--
-- 1. There are no segments whose images overlap in the virtual address space
-- 2. There is at least one LOAD segment
-- 3. The size and file offset of the program header segment don't depend on
--    the virtual address chosen
choosePHDRSegmentAddress ::
  (w ~ MM.ArchAddrWidth arch, E.ElfWidthConstraints w) =>
  proxy arch ->
  E.Elf w ->
  ElfRewriter lm arch (E.ElfWordType w)
choosePHDRSegmentAddress _proxy elf = do
  let phdrs = E.allPhdrs (E.elfLayout elf)

  -- A high (hopefully unused), page-aligned address
  let defaultAddress = 0x900000

  -- To figure out where to put this new segment, we'll need to know its offset
  -- and how big it is, so we first append it at an arbitrary address and get
  -- those values.
  let fakePhdrSeg = phdrSegment (nextSegmentIndex elf) defaultAddress
  ((), fakeELF) <- appendSegment fakePhdrSeg elf
  let fakePhdrs = E.allPhdrs (E.elfLayout fakeELF)
  fakePhdrSegment <-
    case filter ((== E.PT_PHDR) . E.phdrSegmentType) fakePhdrs of
      [seg] -> pure seg
      phdrs' ->
        fail $ "Internal error: Wrong number of PT_PHDR segments: " ++ show phdrs'
  let requiredSize = E.phdrMemSize fakePhdrSegment
  let E.FileOffset projectedOffset = E.phdrFileStart fakePhdrSegment

  -- Call out to 'findSpaceForPhdrs' to find a good address, fail if one can't
  -- be found.
  let segInfos = mapMaybe makeLoadSegmentInfo phdrs
  case NEL.nonEmpty segInfos of
    Nothing -> fail (unlines ("Internal error: No LOAD segments?" : map show phdrs))
    Just segmentInfos ->
      case findSpaceForPHDRs segmentInfos projectedOffset requiredSize of
        Nothing ->
          -- This is fine in practice if there's no thread-local storage.
          if null $ filter ((== E.PT_TLS) . E.phdrSegmentType) phdrs
          then pure defaultAddress
          else fail $ unlines $
                 [ "Internal error: Unable to find space for PHDR segment"
                 , "Offset of PHDR segment: " ++ show projectedOffset
                 , "Size of PHDR segment: " ++ show requiredSize
                 ] ++ map show phdrs
        Just addr -> pure addr

-- | Count the number of program headers (i.e., entries in the PHDR table)
--
-- This is really just the number of segments, but 'E.elfSegmentCount' is not
-- accurate anymore, as elf-edit has special handling for the GNU_STACK segment
-- and RELRO regions.
programHeaderCount :: E.Elf w -> Int
programHeaderCount e = sum [ E.elfSegmentCount e
                           , if isJust (E.elfGnuStackSegment e) then 1 else 0
                           , length (E.elfGnuRelroRegions e)
                           ]

-- | Append a segment to the current ELF binary
--
-- This function checks the alignment requirement of the segment and adds the
-- necessary padding to maintain the alignment constraints.
appendSegment :: ( w ~ MM.ArchAddrWidth arch
                 , E.ElfWidthConstraints w
                 )
              => E.ElfSegment w
              -> E.Elf w
              -> ElfRewriter lm arch ((), E.Elf w)
appendSegment seg e = do
  let layout = E.elfLayout e
  let currentOffset = E.elfLayoutSize layout
  let bytes = E.elfLayoutBytes layout
  assertM (LBS.length bytes == fromIntegral currentOffset)
  -- The sz is the current offset (if we were to append the segment right here)
  --
  -- We need to make sure that the actual offset and the virtual address of the
  -- segment are congruent (w.r.t. the segment alignment), so we'll insert some
  -- padding data if necessary
  let align = E.elfSegmentAlign seg
  let desiredOffset = alignValue currentOffset align
  let paddingBytes = desiredOffset - currentOffset
  let paddingRegion = E.ElfDataRaw (B.replicate (fromIntegral paddingBytes) 0)
  let newRegions = if paddingBytes > 0 then [ paddingRegion, E.ElfDataSegment seg ] else [ E.ElfDataSegment seg ]
  return ((), e L.& E.elfFileData L.%~ (`mappend` Seq.fromList newRegions))

-- | Create a fresh segment containing only the PHDR table
--
-- Requires that we've already made space in the segment index space (by e.g.,
-- incrementing all of the other segment indexes past @idx@).
--
-- The spec says that if there's a PHDR segment, it must be before any loadable
-- segment in the table, so loadable segments should all have indices greater
-- than @idx@.
phdrSegment :: (E.ElfWidthConstraints w)
            => Word16
            -> E.ElfWordType w
            -> E.ElfSegment w
phdrSegment idx addr =
  let alignedAddr = alignValue addr (fromIntegral pageAlignment)
      containerSegment = E.ElfSegment
        -- Why not E.PT_NULL here? Answer: glibc really, *really* wants the program
        -- headers to be visible in its mapped memory somewhere. So we definitely
        -- have to add them as part of a loadable segment.
        { E.elfSegmentType = E.PT_LOAD
        , E.elfSegmentFlags = E.pf_r
        -- Our caller expects the index of the container to be the
        -- largest index of any segment defined here.
        , E.elfSegmentIndex = idx + 1
        , E.elfSegmentVirtAddr = alignedAddr
        , E.elfSegmentPhysAddr = alignedAddr
        , E.elfSegmentAlign = fromIntegral pageAlignment
        , E.elfSegmentMemSize = E.ElfRelativeSize 0
        , E.elfSegmentData = Seq.singleton (E.ElfDataSegment containedSegment)
        }
      containedSegment = containerSegment
        { E.elfSegmentType = E.PT_PHDR
        , E.elfSegmentIndex = idx
        , E.elfSegmentData = Seq.singleton E.ElfDataSegmentHeaders
        }
  in containerSegment

incrementSegmentNumber ::
  (Monad m) =>
  (E.ElfSegment w -> Bool) ->
  E.SegmentIndex ->
  E.ElfSegment w ->
  m (E.ElfSegment w)
incrementSegmentNumber predicate n seg =
  return $
    if predicate seg
    then seg { E.elfSegmentIndex = E.elfSegmentIndex seg + n }
    else seg

buildSymbolMap :: (w ~ MM.ArchAddrWidth arch, Integral (E.ElfWordType w), MM.MemWidth w)
               => E.Elf w
               -> ElfRewriter lm arch (RE.SymbolMap arch)
buildSymbolMap elf = return . flip foldMap (E._elfFileData elf) $ \case
  E.ElfDataSymtab table -> flip foldMap (E.elfSymbolTableEntries table) $ \case
    -- dynamically linked functions are all reported as having address 0; let's
    -- skip those so we don't try to dereference a null pointer later when
    -- converting from ConcreteAddress to MemSegmentOff
    E.EST { E.steType = E.STT_FUNC
          , E.steValue = v
          , E.steName = n
          } | v /= 0 -> Map.singleton (RA.concreteFromAbsolute (fromIntegral v)) n
    _ -> mempty
  _ -> mempty

-- | Build a new symbol table based on the one in the original ELF file
--
-- Copy all of the entries to the new table, but update them to the equivalent
-- blocks in the rewritten binary (as applicable)
buildNewSymbolTable :: (w ~ MM.ArchAddrWidth arch, E.ElfWidthConstraints w, MM.MemWidth w)
                    => Word16
                    -> E.ElfSectionIndex
                    -> RA.ConcreteAddress arch
                    -> RE.NewSymbolsMap arch
                    -> [(RA.ConcreteAddress arch, RA.ConcreteAddress arch)]
                    -> E.ElfSymbolTable (E.ElfWordType w)
                    -- ^ The original symbol table
                    -> E.Elf w
                    -> ElfRewriter lm arch (E.ElfSymbolTable (E.ElfWordType w))
buildNewSymbolTable textSecIdx extraTextSecIdx layoutAddr newSyms addrMap baseTable elf = do
  let newEnts = newEntries (toMap (E.elfSymbolTableEntries baseTable))
  let redirections = redirs
  let tableEntries = V.concat [ E.elfSymbolTableEntries baseTable, V.fromList redirections, newEnts ]
  return $ baseTable { E.elfSymbolTableEntries = tableEntries
                     , E.elfSymbolTableIndex = nextSectionIndex elf
                     }
  where
    toMap      t = Map.fromList [ (E.steValue e, e) | e <- V.toList t ]
    redirectionMap = Map.fromList addrMap
    redirs = [ newFromEntry textSecIdx extraTextSecIdx layoutAddr e redirectedAddr (E.steName e)
             | e <- V.toList (E.elfSymbolTableEntries baseTable)
             , redirectedAddr <- maybeToList (Map.lookup (RA.concreteFromAbsolute (fromIntegral (E.steValue e))) redirectionMap)
             ]
    newEntries t = V.fromList   [ newFromEntry textSecIdx extraTextSecIdx layoutAddr e ca nm
                                | (ca, (oa, nm)) <- Map.toList newSyms
                                , e <- maybeToList $! Map.lookup (fromIntegral (RA.absoluteAddress oa)) t
                                ]

-- | Increment the segment numbers for segments that elf-edit handles specially
--
-- During one stage of the rewriting, we need to increment the segment number of
-- each segment in the binary so that we can reserve segment 0 for a new PHDR
-- table.  For most segments, that is a straightforward traversal over segments.
-- However, elf-edit handles the GNU_STACK and Relro segments specially.  This
-- function acts as a traversal over these special segments and increments their
-- segment numbers.
fixOtherSegmentNumbers :: (Monad m) => E.SegmentIndex -> E.Elf w -> m (E.Elf w)
fixOtherSegmentNumbers n e =
  return e { E.elfGnuStackSegment = fmap updateStackIndex (E.elfGnuStackSegment e)
           , E.elfGnuRelroRegions = fmap updateRelroIndex (E.elfGnuRelroRegions e)
           }
  where
    updateStackIndex gs = gs { E.gnuStackSegmentIndex = E.gnuStackSegmentIndex gs + n }
    updateRelroIndex rr = rr { E.relroSegmentIndex = E.relroSegmentIndex rr + n
                             , E.relroRefSegmentIndex = E.relroRefSegmentIndex rr + n
                             }

-- | Get the current symbol table
getBaseSymbolTable :: (w ~ MM.ArchAddrWidth arch)
                   => E.Elf w
                   -> ElfRewriter lm arch (Maybe (E.ElfSymbolTable (E.ElfWordType w)))
getBaseSymbolTable = return . listToMaybe . E.elfSymtab

-- | Map an original symbol table entry into the new text section
newFromEntry :: (w ~ MM.ArchAddrWidth arch, MM.MemWidth w, E.ElfWidthConstraints w)
             => Word16
             -> E.ElfSectionIndex
             -> RA.ConcreteAddress arch
             -> E.ElfSymbolTableEntry (E.ElfWordType w)
             -> RA.ConcreteAddress arch
             -> B.ByteString
             -> E.ElfSymbolTableEntry (E.ElfWordType w)
newFromEntry textSecIdx extraTextSecIdx layoutAddr e addr nm = e
  { E.steName  = "__renovate_" `B.append` nm
  , E.steValue = fromIntegral absAddr
  , E.steIndex = if absAddr >= RA.absoluteAddress layoutAddr
                 then extraTextSecIdx
                 else E.ElfSectionIndex textSecIdx
  }
  where
    absAddr = RA.absoluteAddress addr

-- | Replace all of the dynamically-sized data regions in the ELF file with padding.
--
-- By dynamically-sized, we mean sections whose sizes will change if
-- we add a new section or segment.  These sections are the section
-- and segment tables, the section name table, and the symbol table.
--
-- We replace them with padding so that none of the original contents
-- of the binary need to move.  We will re-create these sections at
-- the end of the binary when we have finished rewriting it.
--
-- NOTE: We are operating under the constraint that the program headers must
-- reside in the first loadable segment.
--
-- NOTE: In elf-edit, the program header table (PHDR) is called
-- 'E.ElfDataSegmentHeaders'
padDynamicDataRegions :: (w ~ MM.ArchAddrWidth arch, Integral (E.ElfWordType w)) => E.Elf w -> ElfRewriter lm arch ((), E.Elf w)
padDynamicDataRegions e = do
  let layout0 = E.elfLayout e
  ((),) <$> E.traverseElfDataRegions (replaceSectionWithPadding layout0 isDynamicDataRegion) e
  where
    isDynamicDataRegion r =
      case r of
        E.ElfDataSegmentHeaders -> True
        E.ElfDataSectionHeaders -> True
        E.ElfDataSectionNameTable _ -> True
        E.ElfDataSymtab {} -> True
        E.ElfDataStrtab {} -> True
        _ -> False

-- | Append a 'E.ElfDataRegion' to an 'E.Elf' file, adding any necessary padding
-- to maintain the specified alignment.
appendDataRegion :: (w ~ MM.ArchAddrWidth arch, Ord (E.ElfWordType w), Integral (E.ElfWordType w))
                 => E.ElfDataRegion w
                 -> Int
                 -> E.Elf w
                 -> ElfRewriter lm arch ((), E.Elf w)
appendDataRegion r align e = do
  let layout = E.elfLayout e
  let currentOffset = E.elfLayoutSize layout
  let alignedOffset = alignValue currentOffset (fromIntegral align)
  let paddingBytes = alignedOffset - currentOffset
  let paddingRegion = E.ElfDataRaw (B.replicate (fromIntegral paddingBytes) 0)
  let dats = if paddingBytes > 0 then [ paddingRegion, r ] else [ r ]
  return ((), e L.& E.elfFileData L.%~ (`mappend` Seq.fromList dats))

-- | Append the necessary program header data onto the end of the ELF file.
--
-- This includes: section headers, string table, and the section name table.
--
-- NOTE: This explicitly does not include the segment headers (PHDRs), which we
-- put that in the binary last.
--
-- NOTE: It also doesn't include the symbol table, since we already put that in
-- place earlier.
appendHeaders :: (w ~ MM.ArchAddrWidth arch, Show (E.ElfWordType w), Bits (E.ElfWordType w), Integral (E.ElfWordType w))
              => E.Elf w
              -> ElfRewriter lm arch ((), E.Elf w)
appendHeaders elf = do
  let shstrtabidx = nextSectionIndex elf
  let strtabidx = shstrtabidx + 1
--  traceM $ printf "shstrtabidx = %d" shstrtabidx
  let elfData = [ E.ElfDataSectionHeaders
                , E.ElfDataSectionNameTable shstrtabidx
                , E.ElfDataStrtab strtabidx
                ]
  return ((), elf L.& E.elfFileData L.%~ (`mappend` Seq.fromList elfData))

-- | Find the next available section index
--
-- This function does *not* assume that section indexes are allocated densely -
-- it will find the first available index (starting from 0).
nextSectionIndex :: (Bits (E.ElfWordType s), Show (E.ElfWordType s), Integral (E.ElfWordType s)) => E.Elf s -> Word16
nextSectionIndex e = firstAvailable 0 indexes
  where
    indexes  = Map.keys (E.elfLayout e L.^. E.shdrs)

    firstAvailable ix [] = ix
    firstAvailable ix (next:rest)
      | ix == next = firstAvailable (ix + 1) rest
      | otherwise = ix

-- | Traverse a 'E.ElfLayout' and, for any data regions matching the predicate
-- @shouldReplace@, substitute a raw data region of just zero bytes.
replaceSectionWithPadding :: (w ~ MM.ArchAddrWidth arch, Integral (E.ElfWordType w))
                          => E.ElfLayout w
                          -> (E.ElfDataRegion w -> Bool)
                          -> E.ElfDataRegion w
                          -> ElfRewriter lm arch (E.ElfDataRegion w)
replaceSectionWithPadding layout shouldReplace r
  | not (shouldReplace r) = return r
  | otherwise = do
--      traceM ("Overwriting section " ++ show (elfDataRegionName r))
      riOverwrittenRegions L.%= ((elfDataRegionName r, fromIntegral sz):)
      return (E.ElfDataRaw (B.replicate paddingBytes 0))
  where
    sz = E.elfRegionFileSize layout r
    paddingBytes = fromIntegral sz

elfDataRegionName :: E.ElfDataRegion s -> String
elfDataRegionName r =
  case r of
    E.ElfDataElfHeader        -> "ElfHeader"
    E.ElfDataSegmentHeaders   -> "SegmentHeaders"
    E.ElfDataSegment seg      -> printf "Segment(%s:%d)" (show (E.elfSegmentType seg)) (E.elfSegmentIndex seg)
    E.ElfDataSectionHeaders   -> "SectionHeaders"
    E.ElfDataSectionNameTable _ -> "SectionNameTable"
    E.ElfDataGOT _            -> "GOT"
    E.ElfDataSection sec      -> printf "Section(%s)" (C8.unpack (E.elfSectionName sec))
    E.ElfDataRaw _            -> "RawData"
    E.ElfDataStrtab {}        -> "Strtab"
    E.ElfDataSymtab {}        -> "Symtab"

-- | Overwrite the original text section with some new contents (@newBytes@).
overwriteTextSection :: (w ~ MM.ArchAddrWidth arch, Integral (E.ElfWordType w)) => B.ByteString -> E.Elf w -> ElfRewriter lm arch ((), E.Elf w)
overwriteTextSection newBytes e = do
  ((), ) <$> E.elfSections doOverwrite e
  where
    doOverwrite sec
      | E.elfSectionName sec /= C8.pack ".text" = return sec
      | otherwise = do
        when (B.length newBytes /= fromIntegral (E.elfSectionSize sec)) $ do
          C.throwM (RewrittenTextSectionSizeMismatch (B.length newBytes) (fromIntegral (E.elfSectionSize sec)))
        return sec { E.elfSectionData = newBytes
                   , E.elfSectionSize = fromIntegral (B.length newBytes)
                   }

-- | Allocate a new text section (with the name ".extratext")
--
-- This function assumes that the start address it is given is already aligned.
-- It specifies the alignment as 'newTextAlign', and computes the next available
-- section index.
newTextSection :: ( w ~ MM.ArchAddrWidth arch
                  , Num (E.ElfWordType w)
                  , Show (E.ElfWordType w)
                  , Bits (E.ElfWordType w)
                  , Integral (E.ElfWordType w)
                  )
               => E.ElfWordType w
               -> B.ByteString
               -> E.Elf w
               -> ElfRewriter lm arch (E.ElfSectionIndex, E.ElfSection (E.ElfWordType w))
newTextSection startAddr bytes e = do
  let newTextIdx = nextSectionIndex e
  let sec = E.ElfSection { E.elfSectionName = C8.pack ".extratext"
                         , E.elfSectionType = E.SHT_PROGBITS
                         , E.elfSectionFlags = E.shf_alloc .|. E.shf_execinstr
                         , E.elfSectionAddr = startAddr
                         , E.elfSectionSize = fromIntegral (B.length bytes)
                         , E.elfSectionLink = 0
                         , E.elfSectionInfo = 0
                         , E.elfSectionAddrAlign = 32
                         , E.elfSectionEntSize = 0
                         , E.elfSectionData = bytes
                         , E.elfSectionIndex = newTextIdx
                         }
  return (E.ElfSectionIndex newTextIdx, sec)

newDataSection :: (Integral (E.ElfWordType w), Show (E.ElfWordType w), Bits (E.ElfWordType w))
               => E.ElfWordType w
               -> B.ByteString
               -> E.Elf w
               -> ElfRewriter lm arch (E.ElfSection (E.ElfWordType w))
newDataSection baseDataAddr bytes e = do
  let newDataIdx = nextSectionIndex e
  let sec = E.ElfSection { E.elfSectionName = C8.pack ".extradata"
                         , E.elfSectionType = E.SHT_PROGBITS
                         , E.elfSectionFlags = E.shf_alloc .|. E.shf_write
                         , E.elfSectionAddr = baseDataAddr
                         , E.elfSectionSize = fromIntegral (B.length bytes)
                         , E.elfSectionLink = 0
                         , E.elfSectionInfo = 0
                         , E.elfSectionAddrAlign = 32
                         , E.elfSectionEntSize = 0
                         , E.elfSectionData = bytes
                         , E.elfSectionIndex = newDataIdx
                         }
  return sec

newDataSegment :: (Integral (E.ElfWordType w), Show (E.ElfWordType w))
               => E.ElfSection (E.ElfWordType w)
               -> E.Elf w
               -> ElfRewriter lm arch (E.ElfSegment w)
newDataSegment section e =
  return E.ElfSegment { E.elfSegmentType = E.PT_LOAD
                      , E.elfSegmentFlags = E.pf_r .|. E.pf_w
                      , E.elfSegmentIndex = nextSegmentIndex e
                      , E.elfSegmentVirtAddr = E.elfSectionAddr section
                      , E.elfSegmentPhysAddr = E.elfSectionAddr section
                      , E.elfSegmentAlign = fromIntegral newTextAlign
                      , E.elfSegmentMemSize = E.ElfAbsoluteSize (fromIntegral (E.elfSectionSize section))
                      , E.elfSegmentData = Seq.singleton (E.ElfDataSection section)
                      }

-- | Allocate a fresh segment that is executable and loadable
--
-- It assigns the next available segment number and assumes that the address it
-- is given (@startAddr@) is already correctly aligned.
newExecutableSegment :: ( w ~ MM.ArchAddrWidth arch
                        , E.ElfWidthConstraints w
                        )
                     => E.ElfWordType w
                     -> Seq.Seq (E.ElfDataRegion w)
                     -> E.Elf w
                     -> ElfRewriter lm arch (E.ElfSegment w)
newExecutableSegment startAddr contents e = do
  return $ E.ElfSegment { E.elfSegmentType = E.PT_LOAD
                        , E.elfSegmentFlags = E.pf_r .|. E.pf_x
                        , E.elfSegmentIndex = nextSegmentIndex e
                        , E.elfSegmentVirtAddr = startAddr
                        , E.elfSegmentPhysAddr = startAddr
                        , E.elfSegmentAlign = fromIntegral newTextAlign
                        , E.elfSegmentMemSize = E.ElfRelativeSize 0
                        , E.elfSegmentData = contents
                        }

-- | Compute the next available segment index
--
-- This assumes that the current segment indexes are densely-assigned.
nextSegmentIndex :: E.Elf w -> Word16
nextSegmentIndex = fromIntegral . programHeaderCount

-- | Apply the instrumentor to the given section (which should be the
-- .text section), while laying out the instrumented version of the
-- code at the @layoutAddr@.
--
-- The return value is (rewrittenTextSection, instrumentedBytes,
-- newDataSection).  There could be no new data section if none is
-- required.
--
-- Note that the layout of .text section will change exactly what this
-- function does.  We start instrumenting from @main@ (for now) and do
-- not give the instrumentor access to code that comes before @main@
-- in the .text section.  This implies that library code that comes
-- before @main@ will not be instrumented.  We can change that by
-- simply re-arranging the code at link time such that @main@ comes
-- before the library code.
--
-- As a side effect of running the instrumentor, we get information
-- about how much extra space needs to be reserved in a new data
-- section.  The new data section is rooted at @newGlobalBase@.
instrumentTextSection :: forall w arch binFmt b lm
                       . (w ~ MM.ArchAddrWidth arch,
                          MBL.BinaryLoader arch binFmt,
                          B.InstructionConstraints arch,
                          Typeable arch,
                          Integral (E.ElfWordType w),
                          16 <= w,
                          MS.SymArchConstraints arch)
                      => RenovateConfig arch binFmt (AnalyzeAndRewrite lm) b
                      -> C.HandleAllocator
                      -> MBL.LoadedBinary arch binFmt
                      -- ^ The memory space
                      -> (RA.ConcreteAddress arch, RA.ConcreteAddress arch)
                      -- ^ The address of the (start, end) of the text section
                      -> B.ByteString
                      -- ^ The bytes of the text section
                      -> RE.LayoutStrategy
                      -- ^ The strategy to use for laying out instrumented blocks
                      -> RA.ConcreteAddress arch
                      -- ^ The address to lay out the instrumented blocks
                      -> RA.ConcreteAddress arch
                      -- ^ The address to lay out the new data section
                      -> RE.SymbolMap arch
                      -- ^ meta data?
                      -> ElfRewriter lm arch (b arch, B.ByteString, B.ByteString, Maybe B.ByteString, RE.NewSymbolsMap arch, [(RE.ConcreteAddress arch, RE.ConcreteAddress arch)])
instrumentTextSection cfg hdlAlloc loadedBinary textAddrRange@(textSectionStartAddr, textSectionEndAddr) textBytes strat layoutAddr newGlobalBase symmap = do
  withAnalysisEnv cfg hdlAlloc loadedBinary symmap textAddrRange $ \aenv -> do
    let isa = analysisISA aenv
    let mem = MBL.memoryImage (analysisLoadedBinary aenv)
    let blockInfo = analysisBlockInfo aenv
    let blocks = L.sortBy (O.comparing B.concreteBlockAddress) (R.biBlocks blockInfo)
    let (symAlloc1, baseSymBlocks) = RS.symbolizeBasicBlocks isa mem RS.symbolicAddressAllocator blocks
    let symbolicBlockMap = Map.fromList [ (B.concreteBlockAddress cb, sb)
                                        | (cb, sb) <- baseSymBlocks
                                        ]
    newCodeAddr <- fromIntegral <$> R.asks reSegmentVirtualAddress
    newCodeSize <- fromIntegral <$> R.asks reSegmentMaximumSize
    let rae = RewriterAnalysisEnv { raeEnv = aenv
                                  , raeSymBlockMap = symbolicBlockMap
                                  , raeNewCodeAddress =
                                      RA.concreteFromAbsolute newCodeAddr
                                  , raeNewCodeMaxSize = newCodeSize
                                  }
    case rcAnalysis cfg of
      AnalyzeAndRewrite preAnalyze analyze preRewrite rewrite -> do
        (entryPoint NEL.:| _) <- MBL.entryPoints loadedBinary
        let Just concEntryPoint = RA.concreteFromSegmentOff mem entryPoint
        let cfgs = FR.recoverFunctions isa mem blockInfo
        let internalRwEnv = RW.mkRewriteEnv cfgs concEntryPoint mem blockInfo isa (analysisABI aenv) hdlAlloc
        -- For the combined analysis and rewriter pass, we first run the
        -- analysis to produce a global analysis result.  We then pass that to
        -- an initialization function (provided by the caller) that can do some
        -- rewriter-specific initialization based on the analysis result (e.g.,
        -- allocating new global variable storage).  Finally, we pass both the
        -- analysis result and setup value to the actual rewriter.
        let runrw k = IO.liftIO (RW.runRewriteMT internalRwEnv newGlobalBase symAlloc1 k)
        ((eBlocks, r1), info) <- runrw $ do
          preAnalysisResult <- RW.hoist (preAnalyze rae)
          analysisResult <- IO.liftIO (analyze rae preAnalysisResult)
          setupVal <- RW.hoist (preRewrite rae analysisResult)
          RE.runRewriterT isa mem symmap $ do
            let rewriter = RW.hoist . rewrite rae analysisResult setupVal
            r <- RE.redirect isa blockInfo textAddrRange rewriter mem strat layoutAddr baseSymBlocks
            return (analysisResult, r)
        (analysisResult, (allBlocks, injected, blockPairs)) <- extractOrThrowRewriterResult eBlocks r1
        riRewritePairs L..= map RT.toRewritePair blockPairs
        let s1 = RE.rrState r1
        let newSyms = RE.rwsNewSymbolsMap s1

        -- Record some metrics for later analysis
        riRedirectionDiagnostics L..= F.toList (RD.diagnosticMessages $ RE.rrDiagnostics r1)
        riInstrumentationSites L..= RW.infoSites info
        riLogMsgs L..= RW.logMsgs info
        riStats L..= RE.rwsStats s1
        riRecoveredBlocks L..= Just (SomeConcreteBlocks isa blocks)
        riOutputBlocks L..= Just (SomeConcretizedBlocks isa allBlocks)
        riIncompleteFunctions L..= RM.incompleteFunctions mem blockInfo
        riTransitivelyIncompleteBlocks L..= RM.transitivelyIncompleteBlocks blockInfo
        riTranslationErrors L..= R.biTranslationError blockInfo
        riClassifyFailures L..= R.biClassifyFailure blockInfo
        case cfg of
          RenovateConfig { rcAssembler = asm } -> do
            (overwrittenBytes, instrumentationBytes) <- BA.assembleBlocks mem isa textSectionStartAddr textSectionEndAddr textBytes layoutAddr asm allBlocks injected
            let newDataBytes = mkNewDataSection newGlobalBase info
            return (analysisResult, overwrittenBytes, instrumentationBytes, newDataBytes, newSyms, RE.blockMapping (RE.rwsStats s1))


-- | Helper for handling the error case of `RewriterT`.
extractOrThrowRewriterResult :: Either P.SomeException a
                             -> RE.RewriterResult arch
                             -> ElfRewriter lm arch a
extractOrThrowRewriterResult e r = do
  case e of
    Left exn -> do
      let diagnostics = F.toList (RD.diagnosticMessages $ RE.rrDiagnostics r)
      riRedirectionDiagnostics L..= diagnostics
      C.throwM (RewriterFailure exn diagnostics)
    Right x -> return x

withAnalysisEnv :: forall w arch binFmt callbacks b a lm
                    . (w ~ MM.ArchAddrWidth arch,
                       MBL.BinaryLoader arch binFmt,
                       B.InstructionConstraints arch,
                       Integral (E.ElfWordType w),
                       16 <= w,
                       MS.SymArchConstraints arch)
                   => RenovateConfig arch binFmt callbacks b
                   -> C.HandleAllocator
                   -> MBL.LoadedBinary arch binFmt
                   -- ^ The memory space
                   -> RE.SymbolMap arch
                   -> (RA.ConcreteAddress arch, RA.ConcreteAddress arch)
                   -> (AnalysisEnv arch binFmt -> ElfRewriter lm arch a)
                   -> ElfRewriter lm arch a
withAnalysisEnv cfg hdlAlloc loadedBinary symmap textAddrRange k = do
  elfEntryPoints <- MBL.entryPoints loadedBinary
  let isa = rcISA cfg
  let abi = rcABI cfg
  let archInfo = rcArchInfo cfg loadedBinary
  let recovery = R.Recovery { R.recoveryISA = isa
                            , R.recoveryDis = rcDisassembler cfg
                            , R.recoveryAsm = rcAssembler cfg
                            , R.recoveryArchInfo = archInfo
                            , R.recoveryHandleAllocator = hdlAlloc
                            , R.recoveryBlockCallback = rcBlockCallback cfg
                            , R.recoveryFuncCallback = fmap (second ($ loadedBinary)) (rcFunctionCallback cfg)
                            , R.recoveryRefinement = rcRefinementConfig cfg
                            }
  blockInfo <- IO.liftIO (R.recoverBlocks recovery loadedBinary symmap elfEntryPoints textAddrRange)
  let env = AnalysisEnv { aeLoadedBinary = loadedBinary
                        , aeBlockInfo = blockInfo
                        , aeISA = isa
                        , aeABI = abi
                        , aeHandleAllocator = hdlAlloc
                        }
  k env

mkNewDataSection :: (MM.MemWidth (MM.ArchAddrWidth arch)) => RA.ConcreteAddress arch -> RW.RewriteInfo lm arch -> Maybe B.ByteString
mkNewDataSection baseAddr info = do
  guard (bytes > 0)
  return (B.pack (replicate bytes 0))
  where
    bytes = fromIntegral (RW.nextGlobalAddress info `RA.addressDiff` baseAddr)

data ElfRewriteException = RewrittenTextSectionSizeMismatch Int Int
                         | BlockRecoveryFailure C.SomeException [RD.Diagnostic]
                         | RewriterFailure C.SomeException [RD.Diagnostic]
                         | UnsupportedArchitecture E.ElfMachine
                         | MemoryLoadError String
                         deriving (Typeable)

deriving instance Show ElfRewriteException
instance C.Exception ElfRewriteException
