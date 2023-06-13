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
  RewriterInfo(..),
  RE.SectionInfo(..)
  ) where

import           Control.Applicative
import           Control.Arrow ( second )
import qualified Control.Lens as L
import           Control.Monad ( guard, when, unless )
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
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word ( Word16 )
import qualified GHC.Stack as Stack
import           GHC.TypeLits
import qualified Lumberjack as LJ
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

import qualified Renovate.Analysis.FunctionRecovery as FR
import qualified Renovate.Arch as Arch
import qualified Renovate.Assemble as BA
import           Renovate.BinaryFormat.ELF.BSS ( expandBSS )
import           Renovate.BinaryFormat.ELF.Common
import qualified Renovate.BinaryFormat.ELF.InitialSizes as EIS
import           Renovate.BinaryFormat.ELF.Internal
import           Renovate.BinaryFormat.ELF.Rewriter as Rewriter
import           Renovate.Config
import qualified Renovate.Core.Address as RA
import qualified Renovate.Core.BasicBlock as B
import qualified Renovate.Core.Diagnostic as RCD
import qualified Renovate.Core.Exception as RCE
import qualified Renovate.Core.Layout as RT
import qualified Renovate.ISA as RI
import qualified Renovate.Panic as RP
import qualified Renovate.Recovery as R
import qualified Renovate.Redirect as RE
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
-- 'UnsupportedELFArchitecture' exception is thrown.
--
-- Supported architectures are listed in the Renovate.Arch module hierarchy.
withElfConfig
  :: (C.MonadThrow m)
  => E.SomeElf E.ElfHeaderInfo
  -> [(Arch.Architecture, SomeConfig callbacks b)]
  -> (forall arch . ( MS.SymArchConstraints arch
                    , 16 <= MM.ArchAddrWidth arch
                    , MBL.BinaryLoader arch (E.ElfHeaderInfo (MM.ArchAddrWidth arch))
                    , E.ElfWidthConstraints (MM.ArchAddrWidth arch)
                    , RI.ArchConstraints arch
                    )
                  => RenovateConfig arch (E.ElfHeaderInfo (MM.ArchAddrWidth arch)) callbacks b
                  -> E.ElfHeaderInfo (MM.ArchAddrWidth arch)
                  -> MBL.LoadedBinary arch (E.ElfHeaderInfo (MM.ArchAddrWidth arch))
                  -> m t)
  -> m t
withElfConfig (E.SomeElf e0) configs k = do
  let header = E.header e0
  case (E.headerClass header, E.headerMachine header) of
    (E.ELFCLASS32, E.EM_PPC) ->
      case lookup Arch.PPC32 configs of
        Nothing -> C.throwM (RCE.UnsupportedELFArchitecture E.EM_PPC)
        Just (SomeConfig nr binRep cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @32)
          , Just PC.Refl <- PC.testEquality binRep MBL.Elf32Repr ->
              MBL.loadBinary loadOpts e0 >>= k cfg e0
          | otherwise ->
            RP.panic RP.ELFWriting "withElfConfig" ["Invalid NatRepr for PPC32: " ++ show nr]
    (E.ELFCLASS32, E.EM_ARM) ->
      case lookup Arch.ARM configs of
        Nothing -> C.throwM (RCE.UnsupportedELFArchitecture E.EM_ARM)
        Just (SomeConfig nr binRep cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @32)
          , Just PC.Refl <- PC.testEquality binRep MBL.Elf32Repr ->
              MBL.loadBinary loadOpts e0 >>= k cfg e0
          | otherwise ->
            RP.panic RP.ELFWriting "withElfConfig" ["Invalid NatRepr for AArch32: " ++ show nr]
    (E.ELFCLASS32, mach) -> C.throwM (RCE.UnsupportedELFArchitecture mach)
    (E.ELFCLASS64, E.EM_X86_64) ->
      case lookup Arch.X86_64 configs of
        Nothing -> C.throwM (RCE.UnsupportedELFArchitecture E.EM_X86_64)
        Just (SomeConfig nr binRep cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @64)
          , Just PC.Refl <- PC.testEquality binRep MBL.Elf64Repr ->
              MBL.loadBinary loadOpts e0 >>= k cfg e0
          | otherwise ->
            RP.panic RP.ELFWriting "withElfConfig" ["Invalid NatRepr for X86_64: " ++ show nr]
    (E.ELFCLASS64, E.EM_PPC64) ->
      case lookup Arch.PPC64 configs of
        Nothing -> C.throwM (RCE.UnsupportedELFArchitecture E.EM_PPC64)
        Just (SomeConfig nr binRep cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @64)
          , Just PC.Refl <- PC.testEquality binRep MBL.Elf64Repr ->
              MBL.loadBinary loadOpts e0 >>= k cfg e0
          | otherwise ->
            RP.panic RP.ELFWriting "withElfConfig" ["Invalid NatRepr for PPC64: " ++ show nr]
    (E.ELFCLASS64, mach) -> C.throwM (RCE.UnsupportedELFArchitecture mach)
  where
    loadOpts = MM.defaultLoadOptions { MM.loadOffset = Just 0 }

-- | Apply a rewriter to an ELF file using the chosen layout strategy.
--
-- The 'RT.LayoutStrategy' determines how rewritten basic blocks will be laid
-- out in the new binary file.  If the rewriter succeeds, it returns a new ELF
-- file and some metadata describing the changes made to the file.  Some of the
-- metadata is provided by rewriter passes in the 'RW.RewriteM' environment.
rewriteElf
  :: ( RI.ArchConstraints arch
     , MBL.BinaryLoader arch binFmt
     , Stack.HasCallStack
     , E.ElfWidthConstraints (MM.ArchAddrWidth arch)
     , 16 <= MM.ArchAddrWidth arch
     , MS.SymArchConstraints arch
     )
  => LJ.LogAction IO (RCD.Diagnostic lm)
  -> RenovateConfig arch binFmt (AnalyzeAndRewrite lm) b
  -- ^ The configuration for the rewriter
  -> C.HandleAllocator
  -- ^ A handle allocator for allocating crucible function handles (used for lifting macaw->crucible)
  -> E.ElfHeaderInfo (MM.ArchAddrWidth arch)
  -- ^ The ELF file to rewrite
  -> MBL.LoadedBinary arch binFmt
  -- ^ A representation of the contents of memory of the ELF file
  -- (including statically-allocated data)
  -> RT.LayoutStrategy
  -- ^ The layout strategy for blocks in the new binary
  -> IO (E.Elf (MM.ArchAddrWidth arch), b arch, RewriterInfo arch)
rewriteElf logAction cfg hdlAlloc ehi loadedBinary strat = do
    let (elfParseErrors, e) = E.getElf ehi
    unless (null elfParseErrors) $ do
      LJ.writeLog logAction (RCD.ELFDiagnostic (RCD.ELFParseErrors elfParseErrors))
    ((analysisResult, redirectionResult, blockInfo), rs, env) <- runElfRewriter logAction cfg ehi e $ do
      -- FIXME: Use the symbol map from the loaded binary (which we still need to add)
      symmap <- withCurrentELF buildSymbolMap
      doRewrite cfg hdlAlloc loadedBinary symmap strat
    let ri = RewriterInfo { riRedirectionResult = redirectionResult
                          , riRewritePairs = map RT.toRewritePair (RE.rrConcretizedBlocks redirectionResult)
                          , riSegmentVirtualAddress = reSegmentVirtualAddress env
                          , riBlockInfo = blockInfo
                          }
    return (_rsELF rs, analysisResult, ri)

-- | Run an analysis over an ELF file
--
-- Note that the configuration type is keyed by the 'AnalyzeOnly' tag, which
-- restricts the type of the analysis compared to the rewriting variant.
analyzeElf
  :: ( RI.ArchConstraints arch
     , MBL.BinaryLoader arch binFmt
     , E.ElfWidthConstraints (MM.ArchAddrWidth arch)
     , 16 <= MM.ArchAddrWidth arch
     , MS.SymArchConstraints arch
     )
  => LJ.LogAction IO (RCD.Diagnostic lm)
  -> RenovateConfig arch binFmt AnalyzeOnly b
  -- ^ The configuration for the analysis
  -> C.HandleAllocator
  -> E.ElfHeaderInfo (MM.ArchAddrWidth arch)
  -- ^ The ELF file to analyze
  -> MBL.LoadedBinary arch binFmt
  -- ^ A representation of the contents of memory of the ELF file
  -- (including statically-allocated data)
  -> IO (b arch)
analyzeElf logAction cfg hdlAlloc ehi loadedBinary = do
  let (elfParseErrors, e) = E.getElf ehi
  unless (null elfParseErrors) $ do
    LJ.writeLog logAction (RCD.ELFDiagnostic (RCD.ELFParseErrors elfParseErrors))
  (b, _ri, _env) <- runElfRewriter logAction cfg ehi e $ do
    symmap <- withCurrentELF buildSymbolMap
    textSection <- withCurrentELF (findTextSection (rcTextSectionName cfg))
    let textRange = sectionAddressRange textSection
    withAnalysisEnv logAction cfg hdlAlloc loadedBinary symmap textRange $ \env -> do
      IO.liftIO (aoAnalyze (rcAnalysis cfg) env)
  return b

-- | Extract the 'MM.Memory' from an ELF file.
withMemory
  :: forall w m a arch
     . ( C.MonadThrow m
       , MM.MemWidth w
       , Integral (E.ElfWordType w)
       , w ~ MM.ArchAddrWidth arch
       , MBL.BinaryLoader arch (E.Elf w)
       )
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
  elf <- S.gets _rsELF
  k elf

-- | A wrapper around functions that modify the current ELF file.
--
-- The modification function can return some extra data if desired.  The idea is that this is a signal that
-- a function mutates the ELF file.
--
-- This should be the only function that writes to the current ELF file
modifyCurrentELF :: (w ~ MM.ArchAddrWidth arch) => (E.Elf w -> ElfRewriter lm arch (a, E.Elf w)) -> ElfRewriter lm arch a
modifyCurrentELF k = do
  elf <- S.gets _rsELF
  (res, elf') <- k elf
  S.modify' $ \s -> s { _rsELF = elf' }
  return res

-- | Compute the starting and ending address (address range) of an ELF section
--
-- FIXME: It would be nice to bundle the return value up in an ADT instead of a pair
sectionAddressRange :: (MM.MemWidth (MM.ArchAddrWidth arch), Integral a)
                    => E.ElfSection a
                    -> (RA.ConcreteAddress arch, RA.ConcreteAddress arch)
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
-- 5) Append an empty, writable, loadable segment at an address just after the new PHDRs to work
--    around a QEMU bug.
--
-- TODO:
--  * Handle binaries that already contain a separate PHDR segment (do we need
--    to do anything special to remove it?)
--  * Fix handling of new data segments (reserve a fixed amount of space before the new text)
--  * More carefully analyze alignment requirements (the new PHDRs and new text
--    are currently page aligned - is that necessary?)
doRewrite :: (RI.ArchConstraints arch,
              MBL.BinaryLoader arch binFmt,
              Stack.HasCallStack,
              E.ElfWidthConstraints (MM.ArchAddrWidth arch),
              16 <= MM.ArchAddrWidth arch,
              MS.SymArchConstraints arch)
          => RenovateConfig arch binFmt (AnalyzeAndRewrite lm) b
          -> C.HandleAllocator
          -> MBL.LoadedBinary arch binFmt
          -> RE.SymbolMap arch
          -> RT.LayoutStrategy
          -> ElfRewriter lm arch (b arch, RE.RedirectionResult arch, R.BlockInfo arch)
doRewrite cfg hdlAlloc loadedBinary symmap strat = do
  -- We need to compute the extents of every ElfDataRegion so that we can
  -- replace the ones that will change sizes with appropriate padding
  ehdr <- S.gets _rsInitialELFHeader
  let initialSizes = EIS.computeInitialSizes ehdr

  -- We pull some information from the unmodified initial binary: the text
  -- section, the entry point(s), and original symbol table (if any).
  textSection <- withCurrentELF (findTextSection (rcTextSectionName cfg))
  mBaseSymtab <- withCurrentELF getBaseSymbolTable

  -- Remove (and pad out) the sections whose size could change if we
  -- modify the binary.  We'll re-add them later (see @appendHeaders@).
  --
  -- This modifies the underlying ELF file
  modifyCurrentELF (padDynamicDataRegions initialSizes)

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
  let dataAddr = RA.concreteFromAbsolute (fromIntegral (rcDataLayoutBase cfg))
  let textSectionRange = sectionAddressRange textSection

  ( analysisResult
    , overwrittenBytes
    , instrumentedBytes
    , mNewData
    , redirectionResult
    , blockInfo) <- instrumentTextSection cfg hdlAlloc loadedBinary textSectionRange
                                       (E.elfSectionData textSection) strat layoutAddr dataAddr symmap

  let newSyms = RE.rrNewSymbolsMap redirectionResult
  let addrMap = RE.rrBlockMapping redirectionResult
  let instrumentedByteCount = B.length instrumentedBytes

  when (fromIntegral instrumentedByteCount > newTextSize) $ do
    C.throwM (RCE.InsufficientExtraCodeSpace (toInteger instrumentedByteCount) (toInteger newTextSize))
  logDiagnostic (RCD.ELFDiagnostic (RCD.ELFOriginalTextSize (fromIntegral (B.length overwrittenBytes))))
  logDiagnostic (RCD.ELFDiagnostic (RCD.ELFNewTextSize (fromIntegral instrumentedByteCount)))

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
  modifyCurrentELF (liftElfAction (appendSegment newTextSegment))


  -- Update the symbol table (if there is one)
  --
  -- FIXME: If we aren't updating the symbol table (but there is one), just
  -- preserve the original one.  The current code just throws it away.
  case mBaseSymtab of
    Just baseSymtab
      | rcUpdateSymbolTable cfg -> do
          newSymtab <- buildNewSymbolTable (E.elfSectionIndex textSection) newTextSecIdx layoutAddr newSyms addrMap baseSymtab
          let symbolTableAlignment = 8
          symtabIdx <- withCurrentELF (return . nextSectionIndex)
          logDiagnostic (RCD.ELFDiagnostic (RCD.ELFSectionNewIndex ".symtab" symtabIdx))
          modifyCurrentELF (appendDataRegion (E.ElfDataSymtab symtabIdx newSymtab) symbolTableAlignment)
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
  modifyCurrentELF (liftElfAction (appendSegment newDataSeg))

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
    P.throwM (RCE.TooManyEXIDXSegments (map E.elfSegmentIndex exidxs))
  phdrSegmentIdx <-
    case E.elfSegmentIndex <$> listToMaybe exidxs of
      Nothing -> pure 0
      Just idx | idx == 0 -> pure 1
      Just idx -> P.throwM (RCE.WrongEXIDXIndex idx)

  phdrSegmentAddress <- withCurrentELF choosePHDRSegmentAddress
  let newSegment = phdrSegment phdrSegmentIdx phdrSegmentAddress
  let newSegmentCount = E.elfSegmentIndex newSegment + 1
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
  modifyCurrentELF (liftElfAction (appendSegment newSegment))

  -- Linux calculates the program break based on the virtual address and size of
  -- the LOAD segment with the highest virtual address, whereas QEMU calculates
  -- it based on the virtual address and size of the *writable* LOAD segment with
  -- the highest virtual address. Since our PHDR segment might have a high
  -- virtual address, we have to add a writable load segment after it, so our
  -- programs don't segfault inside QEMU.
  --
  -- https://bugs.launchpad.net/qemu/+bug/1886097
  --
  -- NOTE: This is the size of the PHDRs in the final binary (with our modified
  -- PHDRs added, and the original PHDRs nullified); it cannot use the initial
  -- size snapshots
  phdrActualSize <- withCurrentELF getPHDRMemSize

  lastWritableSegmentIdx <- withCurrentELF (pure . (+1) . nextSegmentIndex)
  let alignedWritableSegAddr = alignValue
            -- Put this new segment immediately after the PHDRs in the virtual
            -- address space
            (E.elfSegmentVirtAddr newSegment + phdrActualSize)
            (fromIntegral pageAlignment)
  let lastWritableSegment = E.ElfSegment
         { E.elfSegmentType = E.PT_LOAD
         , E.elfSegmentFlags = E.pf_w
         , E.elfSegmentIndex = lastWritableSegmentIdx
         , E.elfSegmentVirtAddr = alignedWritableSegAddr
         , E.elfSegmentPhysAddr = alignedWritableSegAddr
         , E.elfSegmentAlign = 1
         , E.elfSegmentMemSize = E.ElfRelativeSize 0
         , E.elfSegmentData = Seq.singleton (E.ElfDataRaw "")
         }
  modifyCurrentELF (liftElfAction (appendSegment lastWritableSegment))
  return (analysisResult, redirectionResult, blockInfo)

-- | Collect the currently-used section numbers in the 'E.Elf' file
--
-- This does not use the 'E.elfSections' combinator, as that skips a few types
-- of section (e.g., symbol tables).
--
-- Note that the data region traversal handles recursion through segments
sectionNumbers :: E.Elf w -> Set.Set Word16
sectionNumbers e = S.execState (E.traverseElfDataRegions recordSectionNumber e) Set.empty
  where
    -- We return the original segment, but are ignoring the return value of the traversal
    recordSectionNumber r = do
      case r of
        E.ElfDataElfHeader -> return ()
        E.ElfDataSegmentHeaders -> return ()
        E.ElfDataSectionHeaders -> return ()
        E.ElfDataSegment {} -> return ()
        E.ElfDataRaw {} -> return ()
        E.ElfDataSection sec -> S.modify' (Set.insert (E.elfSectionIndex sec))
        E.ElfDataSectionNameTable idx -> S.modify' (Set.insert idx)
        E.ElfDataGOT got -> S.modify' (Set.insert (E.elfGotIndex got))
        E.ElfDataStrtab idx -> S.modify' (Set.insert idx)
        E.ElfDataSymtab idx _symtab -> S.modify' (Set.insert idx)
      return r

-- | Mark the segment as a NULL segment if it was a PHDR table
--
-- We need this because we are zeroing out the original PHDR table and building
-- a new one (which will include our new segments)
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
  (w ~ MM.ArchAddrWidth arch, E.ElfWidthConstraints w, Stack.HasCallStack) =>
  E.Elf w ->
  ElfRewriter lm arch (E.ElfWordType w)
choosePHDRSegmentAddress elf = do
  -- The initial virtual address to assign to the provisional PHDR segment.  It
  -- should not actually matter what this is, because we are computing the real
  -- address that will be assigned to the segment.  It does, however, have to
  -- not be too high, otherwise it will "consume" all of the address space after
  -- the existing segments, breaking the search for an unused address.
  --
  -- Given that, 0 seems fine here.
  let defaultAddress = 0

  -- To figure out where to put this new segment, we'll need to know its offset
  -- and how big it is, so we first append it at an arbitrary address and get
  -- those values.
  let fakePhdrSeg = phdrSegment (nextSegmentIndex elf) defaultAddress
  EIS.Extent { EIS.offset = projectedOffset, EIS.size = requiredSize } <- phdrExtentsWith elf fakePhdrSeg

  -- Call out to 'findSpaceForPhdrs' to find a good (virtual) address, fail if
  -- one can't be found.
  (tlssegment, segmentInfos) <- indexLoadableSegments elf fakePhdrSeg
  case findSpaceForPHDRs segmentInfos projectedOffset requiredSize of
    TLSSafeAddress addr -> pure addr
    FallbackAddress addr ->
      -- The fallback address is a free address that does not obey all of the
      -- restrictions necessary for QEMU+TLS compatibility.
      --
      -- This is fine in practice if there's no thread-local storage,
      -- because libc won't need to walk the program headers to initialize
      -- thread-local storage.
      case tlssegment of
        NoTLSSegment -> pure addr
        TLSSegmentIndex {} ->
          P.throwM $ RCE.InsufficientPHDRSpace (fromIntegral projectedOffset) (fromIntegral requiredSize)

-- | Render the given 'E.Elf' into a bytestring and reparse it to get a 'E.ElfHeaderInfo'
renderElfHeader :: (Stack.HasCallStack, P.MonadThrow m)
                => E.Elf w
                -> m (E.ElfHeaderInfo w)
renderElfHeader e0 = do
  let bs = E.renderElf e0
  case E.decodeElfHeaderInfo (LBS.toStrict bs) of
    Left (errOff, msg) ->
      RP.panic RP.ELFWriting "renderElfHeader" [ "Could not decode the current ELF object during rewriting"
                                               , Stack.prettyCallStack Stack.callStack
                                               , "  at offset " ++ show errOff
                                               , "  with error: " ++ msg
                                               ]
    Right (E.SomeElf ehi)
      | Just PC.Refl <- PC.testEquality (E.elfClass e0) (E.headerClass (E.header ehi)) -> return ehi
      | otherwise -> RP.panic RP.ELFWriting "renderElfHeader" [ "Architecture width mismatch on ELF reparsing"
                                                              , Stack.prettyCallStack Stack.callStack
                                                              ]

data TLSSegmentIndex = NoTLSSegment
                     | TLSSegmentIndex Word16

-- | Compute the extents (offsets + sizes + alignment) of every loadable segment
--
-- NOTE that this works by rendering the current ELF file and examining the
-- PHDRs, as this information is not available in an 'E.Elf'.  Because of this,
-- the 'E.Elf' *must* have a PHDR table (i.e., the 'E.ElfDataSegmentHeaders'
-- 'E.ElfDataRegion'), otherwise one will not be generated and the resulting
-- rendered ELF will be garbage.
--
-- To accommodate this, because the 'E.Elf' does not have a PHDR table at this
-- point, we append an interim table to the end (the second argument).
indexLoadableSegments
  :: (E.ElfWidthConstraints w, w ~ MM.ArchAddrWidth arch, Stack.HasCallStack)
  => E.Elf w
  -> E.ElfSegment w
  -- ^ The temporary PHDR segment
  -> ElfRewriter lm arch (TLSSegmentIndex, NEL.NonEmpty (LoadSegmentInfo w))
indexLoadableSegments e phdrSeg = do
  let e' = appendSegment phdrSeg e
  ehi <- renderElfHeader e'
  let segInfos = mapMaybe makeLoadSegmentInfo (E.headerPhdrs ehi)
  case NEL.nonEmpty segInfos of
    Nothing -> P.throwM (RCE.NoLoadableELFSegments (E.headerPhdrs ehi))
    Just segInfos' -> do
      let hastls = foldr findTLSSegment NoTLSSegment (E.headerPhdrs ehi)
      return (hastls, segInfos')

findTLSSegment :: E.Phdr w -> TLSSegmentIndex -> TLSSegmentIndex
findTLSSegment phdr tls =
  case tls of
    TLSSegmentIndex {} -> tls
    NoTLSSegment
      | E.phdrSegmentType phdr == E.PT_TLS -> TLSSegmentIndex (E.phdrSegmentIndex phdr)
      | otherwise -> tls

-- | Compute the size of the PHDRs for the given 'E.Elf' file
--
-- To handle this, we encode the current ELF to bytes and then re-parse it to
-- get back an 'E.ElfHeaderInfo', which is the only way we have to compute these
-- sizes using elf-edit.
--
-- This will throw an exception if the bytestring created by encoding the
-- current 'E.Elf' file cannot be re-decoded.  Arguably, that could be a panic
-- indicating a fundamental error in elf-edit or in the use of it here.
getPHDRMemSize :: (Stack.HasCallStack, P.MonadThrow m) => E.Elf w -> m (E.ElfWordType w)
getPHDRMemSize elf = do
  ehi <- renderElfHeader elf
  let (_off, sz) = E.phdrTableRange ehi
  return sz

-- | Append the given 'E.ElfSegment' to the given 'E.Elf' and compute the
-- extents it would have
--
-- Note that this function does *not* change the current ELF file in the
-- 'ElfRewriter'.  It only speculatively (and purely) modifies the input
-- 'E.Elf'.
--
-- The intent of this function is to help compute a suitable virtual address of
-- the new PHDR table.
--
-- Assumptions:
--
-- 1. After the segment is appended, there is only a single PHDR table (i.e.,
--    there is only one ElfDataSegmentHeaders)
phdrExtentsWith
  :: (w ~ MM.ArchAddrWidth arch, E.ElfWidthConstraints w, Stack.HasCallStack)
  => E.Elf w
  -> E.ElfSegment w
  -> ElfRewriter lm arch (EIS.Extent w)
phdrExtentsWith elf seg = do
  -- FIXME: Assert that there is only a single instance of 'E.ElfDataSegmentHeaders'
  let elf' = appendSegment seg elf
  ehi <- renderElfHeader elf'
  let (E.FileOffset off, sz) = E.phdrTableRange ehi
  return EIS.Extent { EIS.offset = off, EIS.size = sz }

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

liftElfAction :: (E.Elf w -> E.Elf w) -> E.Elf w -> ElfRewriter lm arch ((), E.Elf w)
liftElfAction a e = return ((), a e)

-- | Append a segment to the provided ELF binary
--
-- This function checks the alignment requirement of the segment and adds the
-- necessary padding to maintain the alignment constraints.
appendSegment :: ( E.ElfWidthConstraints w )
              => E.ElfSegment w
              -> E.Elf w
              -> E.Elf w
appendSegment seg e = do
  let bytes = E.renderElf e
  let currentOffset = LBS.length bytes
  -- The sz is the current offset (if we were to append the segment right here)
  --
  -- We need to make sure that the actual offset and the virtual address of the
  -- segment are congruent (w.r.t. the segment alignment), so we'll insert some
  -- padding data if necessary
  let align = E.elfSegmentAlign seg
  let desiredOffset = alignValue currentOffset (fromIntegral align)
  let paddingBytes = desiredOffset - currentOffset
  let paddingRegion = E.ElfDataRaw (B.replicate (fromIntegral paddingBytes) 0)
  let newRegions = if paddingBytes > 0 then [ paddingRegion, E.ElfDataSegment seg ] else [ E.ElfDataSegment seg ]
  e L.& E.elfFileData L.%~ (`mappend` Seq.fromList newRegions)

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
incrementSegmentNumber incrementIf n seg =
  return $
    if incrementIf seg
    then seg { E.elfSegmentIndex = E.elfSegmentIndex seg + n }
    else seg

buildSymbolMap :: (w ~ MM.ArchAddrWidth arch, Integral (E.ElfWordType w), MM.MemWidth w)
               => E.Elf w
               -> ElfRewriter lm arch (RE.SymbolMap arch)
buildSymbolMap elf = return . flip foldMap (E._elfFileData elf) $ \case
  E.ElfDataSymtab _secIdx table -> flip foldMap (E.symtabEntries table) $ \case
    -- dynamically linked functions are all reported as having address 0; let's
    -- skip those so we don't try to dereference a null pointer later when
    -- converting from ConcreteAddress to MemSegmentOff
    E.SymtabEntry { E.steType = E.STT_FUNC
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
                    -> RE.BlockMapping arch
                    -> E.Symtab w
                    -- ^ The original symbol table
                    -> ElfRewriter lm arch (E.Symtab w)
buildNewSymbolTable textSecIdx extraTextSecIdx layoutAddr newSyms addrMap baseTable = do
  let newEnts = newEntries (toMap (E.symtabEntries baseTable))
  let redirections = redirs
  let tableEntries = V.concat [ E.symtabEntries baseTable, V.fromList redirections, newEnts ]
  return $ baseTable { E.symtabEntries = tableEntries
                     -- FIXME: What is this? It seems like it shouldn't be the same as the table size
                     , E.symtabLocalCount = fromIntegral (V.length tableEntries)
                     }
  where
    toMap      t = Map.fromList [ (E.steValue e, e) | e <- V.toList t ]
    redirectionMap = RE.forwardBlockMapping addrMap
    redirs = [ newFromEntry textSecIdx extraTextSecIdx layoutAddr e redirectedAddr (E.steName e)
             | e <- V.toList (E.symtabEntries baseTable)
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
                   -> ElfRewriter lm arch (Maybe (E.Symtab w))
getBaseSymbolTable = return . listToMaybe . E.elfSymtab

-- | Map an original symbol table entry into the new text section
newFromEntry :: (w ~ MM.ArchAddrWidth arch, MM.MemWidth w, E.ElfWidthConstraints w)
             => Word16
             -> E.ElfSectionIndex
             -> RA.ConcreteAddress arch
             -> E.SymtabEntry B.ByteString (E.ElfWordType w)
             -> RA.ConcreteAddress arch
             -> B.ByteString
             -> E.SymtabEntry B.ByteString (E.ElfWordType w)
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
-- NOTE: We have to do this replacement before we change the sizes of anything.
-- We pre-compute all of the sizes based on the 'E.ElfHeaderInfo'.
--
-- NOTE: We are operating under the constraint that the program headers must
-- reside in the first loadable segment.
--
-- NOTE: In elf-edit, the program header table (PHDR) is called
-- 'E.ElfDataSegmentHeaders'
padDynamicDataRegions :: (w ~ MM.ArchAddrWidth arch, Integral (E.ElfWordType w))
                      => EIS.InitialSizes w
                      -> E.Elf w
                      -> ElfRewriter lm arch ((), E.Elf w)
padDynamicDataRegions initialSizes e =
  ((),) <$> E.traverseElfDataRegions (replaceSectionWithPadding (isDynamicDataRegion initialSizes)) e

-- | If the 'E.ElfDataRegion' has a size that depends on the ELF structure
-- (e.g., number of sections or segments), return the extent of that segment.
isDynamicDataRegion :: EIS.InitialSizes w
                    -> E.ElfDataRegion w
                    -> Maybe (EIS.Extent w)
isDynamicDataRegion initialSizes r =
  case r of
    E.ElfDataSegmentHeaders -> return (EIS.programHeaderTable initialSizes)
    E.ElfDataSectionHeaders -> return (EIS.sectionHeaderTable initialSizes)
    E.ElfDataSectionNameTable idx
      | Just xt <- Map.lookup idx (EIS.sectionExtents initialSizes) -> return xt
      | otherwise ->
        RP.panic RP.ELFWriting "isDynamicDataRegion" ["Missing extents for the ElfDataSectionNameTable " ++ show idx]
    E.ElfDataSymtab idx _symtab
      | Just xt <- Map.lookup idx (EIS.sectionExtents initialSizes) -> return xt
      | otherwise ->
        RP.panic RP.ELFWriting "isDynamicDataRegion" ["Missing extents for the ElfDataSymtab " ++ show idx]
    E.ElfDataStrtab idx
      | Just xt <- Map.lookup idx (EIS.sectionExtents initialSizes) -> return xt
      | otherwise ->
        RP.panic RP.ELFWriting "isDynamicDataRegion" ["Missing extents for the ElfDataStrtab " ++ show idx]
    _ -> Nothing

-- | Append a 'E.ElfDataRegion' to an 'E.Elf' file, adding any necessary padding
-- to maintain the specified alignment.
appendDataRegion :: (w ~ MM.ArchAddrWidth arch, Ord (E.ElfWordType w), Integral (E.ElfWordType w))
                 => E.ElfDataRegion w
                 -> Int
                 -> E.Elf w
                 -> ElfRewriter lm arch ((), E.Elf w)
appendDataRegion r align e = do
  let currentOffset = LBS.length (E.renderElf e)
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
  logDiagnostic (RCD.ELFDiagnostic (RCD.ELFSectionNewIndex ".shstrtab" shstrtabidx))
  logDiagnostic (RCD.ELFDiagnostic (RCD.ELFSectionNewIndex ".strtab" strtabidx))
  let elfData = [ E.ElfDataSectionHeaders
                , E.ElfDataSectionNameTable shstrtabidx
                , E.ElfDataStrtab strtabidx
                ]
  return ((), elf L.& E.elfFileData L.%~ (`mappend` Seq.fromList elfData))

-- | Find the next available section index
--
-- This function does *not* assume that section indexes are allocated densely -
-- it will find the first available index (starting from 1).
--
-- NOTE: This used to be zero, but elf-edit now seems to start with index 1
nextSectionIndex :: (Bits (E.ElfWordType s), Show (E.ElfWordType s), Integral (E.ElfWordType s))
                 => E.Elf s
                 -> Word16
nextSectionIndex e = firstAvailable 1 indexes
  where
    indexes  = F.toList (sectionNumbers e)

    firstAvailable ix [] = ix
    firstAvailable ix (next:rest)
      | ix == next = firstAvailable (ix + 1) rest
      | otherwise = ix

-- | Traverse a 'E.ElfLayout' and, for any data regions matching the predicate
-- @shouldReplace@, substitute a raw data region of just zero bytes.
replaceSectionWithPadding :: (w ~ MM.ArchAddrWidth arch, Integral (E.ElfWordType w))
                          => (E.ElfDataRegion w -> Maybe (EIS.Extent w))
                          -> E.ElfDataRegion w
                          -> ElfRewriter lm arch (E.ElfDataRegion w)
replaceSectionWithPadding shouldReplace r
  | Just extents <- shouldReplace r = do
      let sz = EIS.size extents
      let paddingBytes = fromIntegral sz
      logDiagnostic (RCD.ELFDiagnostic (RCD.ELFOverwroteRegion (elfDataRegionName r) (fromIntegral sz)))
      return (E.ElfDataRaw (B.replicate paddingBytes 0))
  | otherwise = return r

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
    doOverwrite sec = do
        secName <- R.asks reTextSectionName
        if E.elfSectionName sec /= C8.pack secName
           then return sec
           else do
               when (B.length newBytes /= fromIntegral (E.elfSectionSize sec)) $ do
                 C.throwM (RCE.RewrittenTextSectionSizeMismatch (toInteger (B.length newBytes)) (toInteger (E.elfSectionSize sec)))
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
  logDiagnostic (RCD.ELFDiagnostic (RCD.ELFSectionNewIndex ".extratext" newTextIdx))
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
  logDiagnostic (RCD.ELFDiagnostic (RCD.ELFSectionNewIndex ".extradata" newDataIdx))
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
instrumentTextSection
  :: forall w arch binFmt b lm
     . (w ~ MM.ArchAddrWidth arch
       , MBL.BinaryLoader arch binFmt
       , RI.ArchConstraints arch
       , Integral (E.ElfWordType w)
       , 16 <= w
       , MS.SymArchConstraints arch
       )
  => RenovateConfig arch binFmt (AnalyzeAndRewrite lm) b
  -> C.HandleAllocator
  -> MBL.LoadedBinary arch binFmt
  -- ^ The memory space
  -> (RA.ConcreteAddress arch, RA.ConcreteAddress arch)
  -- ^ The address of the (start, end) of the text section
  -> B.ByteString
  -- ^ The bytes of the text section
  -> RT.LayoutStrategy
  -- ^ The strategy to use for laying out instrumented blocks
  -> RA.ConcreteAddress arch
  -- ^ The address to lay out the instrumented blocks
  -> RA.ConcreteAddress arch
  -- ^ The address to lay out the new data section
  -> RE.SymbolMap arch
  -- ^ meta data?
  -> ElfRewriter lm arch ( b arch
                         , B.ByteString
                         , B.ByteString
                         , Maybe B.ByteString
                         , RE.RedirectionResult arch
                         , R.BlockInfo arch
                         )
instrumentTextSection cfg hdlAlloc loadedBinary textAddrRange textBytes strat layoutAddr newGlobalBase symmap = do
  logAction <- R.asks reLogAction
  withAnalysisEnv logAction cfg hdlAlloc loadedBinary symmap textAddrRange $ \aenv -> do
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
        let internalRwEnv = RW.mkRewriteEnv logAction cfgs concEntryPoint mem blockInfo isa (analysisABI aenv) hdlAlloc

        -- For the combined analysis and rewriter pass, we first run the
        -- analysis to produce a global analysis result.  We then pass that to
        -- an initialization function (provided by the caller) that can do some
        -- rewriter-specific initialization based on the analysis result (e.g.,
        -- allocating new global variable storage).  Finally, we pass both the
        -- analysis result and setup value to the actual rewriter.
        let runrw k = IO.liftIO (RW.runRewriteM internalRwEnv newGlobalBase symAlloc1 k)
        ((analysisResult, xformedBlocks, injFuncs, injInsns), info) <- runrw $ do
          preAnalysisResult <- preAnalyze rae
          analysisResult <- IO.liftIO (analyze rae preAnalysisResult)
          setupVal <- preRewrite rae analysisResult
          let rewriter = rewrite rae analysisResult setupVal
          xformedBlocks <- RW.instrumentBlocks isa blockInfo textAddrRange rewriter mem baseSymBlocks
          injFuncs <- RW.getInjectedFunctions
          injInsns <- RW.getInjectedInstructions
          return (analysisResult, xformedBlocks, injFuncs, injInsns)

        redirectResult <- RE.runRedirectT logAction isa mem symmap $ do
          RE.redirect blockInfo strat layoutAddr xformedBlocks injFuncs injInsns

        case cfg of
          RenovateConfig { rcAssembler = asm } -> do
            (overwrittenBytes, instrumentationBytes) <- BA.assembleBlocks mem isa textAddrRange textBytes layoutAddr asm redirectResult
            let newDataBytes = mkNewDataSection newGlobalBase info
            return (analysisResult, overwrittenBytes, instrumentationBytes, newDataBytes, redirectResult, blockInfo)

withAnalysisEnv
  :: forall w arch binFmt callbacks b a lm
     . (w ~ MM.ArchAddrWidth arch
       , MBL.BinaryLoader arch binFmt
       , RI.ArchConstraints arch
       , Integral (E.ElfWordType w)
       , 16 <= w
       , MS.SymArchConstraints arch
       )
  => LJ.LogAction IO (RCD.Diagnostic lm)
  -> RenovateConfig arch binFmt callbacks b
  -> C.HandleAllocator
  -> MBL.LoadedBinary arch binFmt
  -- ^ The memory space
  -> RE.SymbolMap arch
  -> (RA.ConcreteAddress arch, RA.ConcreteAddress arch)
  -> (AnalysisEnv arch binFmt -> ElfRewriter lm arch a)
  -> ElfRewriter lm arch a
withAnalysisEnv logAction cfg hdlAlloc loadedBinary symmap textAddrRange k = do
  elfEntryPoints <- MBL.entryPoints loadedBinary
  let isa = rcISA cfg
  let abi = rcABI cfg
  let archInfo = rcArchInfo cfg loadedBinary
  let recovery = R.Recovery { R.recoveryISA = isa
                            , R.recoveryDis = rcDisassembler cfg
                            , R.recoveryAsm = rcAssembler cfg
                            , R.recoveryArchInfo = archInfo
                            , R.recoveryHandleAllocator = hdlAlloc
                            , R.recoveryFuncCallback = fmap (second ($ loadedBinary)) (rcFunctionCallback cfg)
                            , R.recoveryRefinement = rcRefinementConfig cfg
                            }
  blockInfo <- IO.liftIO (R.recoverBlocks logAction recovery loadedBinary symmap elfEntryPoints textAddrRange)
  let env = AnalysisEnv { aeLoadedBinary = loadedBinary
                        , aeBlockInfo = blockInfo
                        , aeISA = isa
                        , aeABI = abi
                        , aeHandleAllocator = hdlAlloc
                        }
  k env

mkNewDataSection
  :: (MM.MemWidth (MM.ArchAddrWidth arch))
  => RA.ConcreteAddress arch
  -> RW.RewriteInfo arch
  -> Maybe B.ByteString
mkNewDataSection baseAddr info = do
  guard (bytes > 0)
  return (B.pack (replicate bytes 0))
  where
    bytes = fromIntegral (RW.nextGlobalAddress info `RA.addressDiff` baseAddr)

