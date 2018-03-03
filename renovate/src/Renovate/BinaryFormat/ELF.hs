{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
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
  riRedirectionDiagnostics,
  riBlockRecoveryDiagnostics,
  riBlockMapping,
  RenovateConfig(..),
  RewriterInfo,
  SomeBlocks(..)
  ) where

import           GHC.Generics ( Generic )
import           GHC.TypeLits ( KnownNat )

import           Control.Applicative
import qualified Control.Lens as L
import           Control.Monad ( guard, when )
import qualified Control.Monad.Catch as C
import qualified Control.Monad.Catch.Pure as P
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.State.Strict as S
import           Data.Bits ( Bits, (.|.) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Foldable as F
import qualified Data.Functor.Identity as I
import qualified Data.Generics.Product as GL
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import           Data.Maybe ( catMaybes, maybeToList, listToMaybe, mapMaybe )
import qualified Data.Vector as V
import           Data.Monoid
import qualified Data.Sequence as Seq
import           Data.Typeable ( Typeable )
import           Data.Word ( Word16, Word32, Word64 )
import           Text.Printf ( printf )

import           Prelude

import qualified Data.ElfEdit as E
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.ElfLoader as MM
import qualified Data.Parameterized.Classes as PC
import qualified Data.Parameterized.NatRepr as NR

import qualified Renovate.Address as RA
import qualified Renovate.Analysis.FunctionRecovery as FR
import qualified Renovate.Arch as Arch
import qualified Renovate.BasicBlock as B
import qualified Renovate.BasicBlock.Assemble as BA
import           Renovate.Config
import qualified Renovate.Diagnostic as RD
import qualified Renovate.ISA as ISA
import qualified Renovate.Recovery as R
import qualified Renovate.Redirect as RE
import qualified Renovate.Redirect.Monad as RM
import qualified Renovate.Rewrite as RW

import           Debug.Trace
debug :: a -> String -> a
debug = flip trace
-- | The system page alignment (assuming 4k pages)
pageAlignment :: Word32
pageAlignment = 0x1000

-- | Statistics gathered and diagnostics generated during the
-- rewriting phase.
data RewriterInfo w =
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
               , _riInstrumentationSites :: [RW.RewriteSite w]
               , _riELF :: E.Elf w
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
               , _riBlockMapping :: [(RA.ConcreteAddress w, RA.ConcreteAddress w)]
               -- ^ A mapping of original block addresses to rewritten block addresses
               }
  deriving (Generic)

data SomeBlocks = forall i a w
                . (MM.MemWidth w, ISA.InstructionConstraints i a)
                => SomeBlocks (ISA.ISA i a w) [B.ConcreteBlock i w]

-- | Apply an instrumentation pass to the code in an ELF binary,
-- rewriting the binary.
--
-- This will overwrite the original .text section with redirections to
-- a new segment named 'brittle'.
--
-- It applies the correct rewriter config for the architecture
-- specified by the ELF file, if there is an appropriate rewriter
-- configuration.  If not, an error is returned.  The architecture is
-- determined by examining metadata in the ELF file that lists the
-- machine architecture.  Supported architectures are listed in the
-- Renovate.Arch module hierarchy.

withElfConfig :: (C.MonadThrow m)
              => E.SomeElf E.Elf
              -- ^ The ELF file to analyze
              -> [(Arch.Architecture, SomeConfig b)]
              -> (forall i a w arch . (R.ArchBits arch w,
                                       Typeable w,
                                       KnownNat w,
                                       E.ElfWidthConstraints w,
                                       ISA.InstructionConstraints i a)
                                   => RenovateConfig i a w arch b
                                   -> E.Elf w
                                   -> MM.Memory w
                                   -> m t)
              -> m t
withElfConfig e0 configs k =
  case (e0, withElf e0 E.elfMachine) of
    (E.Elf32 e, E.EM_PPC) ->
      case lookup Arch.PPC32 configs of
        Nothing -> C.throwM (UnsupportedArchitecture E.EM_PPC)
        Just (SomeConfig nr cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @32) ->
            withMemory e (k cfg e)
          | otherwise -> error ("Invalid NatRepr for PPC32: " ++ show nr)
    (E.Elf32 _, mach) -> C.throwM (UnsupportedArchitecture mach)
    (E.Elf64 e, E.EM_X86_64) ->
      case lookup Arch.X86_64 configs of
        Nothing -> C.throwM (UnsupportedArchitecture E.EM_X86_64)
        Just (SomeConfig nr cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @64) ->
              withMemory e (k cfg e)
          | otherwise -> error ("Invalid NatRepr for X86_64: " ++ show nr)
    (E.Elf64 e, E.EM_PPC64) ->
      case lookup Arch.PPC64 configs of
        Nothing -> C.throwM (UnsupportedArchitecture E.EM_PPC64)
        Just (SomeConfig nr cfg)
          | Just PC.Refl <- PC.testEquality nr (NR.knownNat @64) ->
            withMemory e (k cfg e)
          | otherwise -> error ("Invalid NatRepr for PPC64: " ++ show nr)
    (E.Elf64 _, mach) -> C.throwM (UnsupportedArchitecture mach)

-- | Apply a rewriter to an ELF file using the chosen layout strategy.
--
-- The 'RE.LayoutStrategy' determines how rewritten basic blocks will be laid
-- out in the new binary file.  If the rewriter succeeds, it returns a new ELF
-- file and some metadata describing the changes made to the file.  Some of the
-- metadata is provided by rewriter passes in the 'RW.RewriteM' environment.
rewriteElf :: (ISA.InstructionConstraints i a,
               E.ElfWidthConstraints w,
               KnownNat w,
               Typeable w,
               R.ArchBits arch w)
           => RenovateConfig i a w arch b
           -- ^ The configuration for the rewriter
           -> E.Elf w
           -- ^ The ELF file to rewrite
           -> MM.Memory w
           -- ^ A representation of the contents of memory of the ELF file
           -- (including statically-allocated data)
           -> RE.LayoutStrategy
           -- ^ The layout strategy for blocks in the new binary
           -> IO (E.Elf w, b, RewriterInfo w)
rewriteElf cfg e mem strat = do
    (analysisResult, ri) <- S.runStateT (unElfRewrite act) (emptyRewriterInfo e)
    return (_riELF ri, analysisResult, ri)
  where
    act = do
      symmap <- withCurrentELF (buildSymbolMap mem)
      doRewrite cfg mem symmap strat

analyzeElf :: (ISA.InstructionConstraints i a,
               E.ElfWidthConstraints w,
               KnownNat w,
               Typeable w,
               R.ArchBits arch w)
           => RenovateConfig i a w arch b
           -- ^ The configuration for the analysis
           -> E.Elf w
           -- ^ The ELF file to analyze
           -> MM.Memory w
           -- ^ A representation of the contents of memory of the ELF file
           -- (including statically-allocated data)
           -> IO (b, [RM.Diagnostic])
analyzeElf cfg e mem = do
    (b, ri) <- S.runStateT (unElfRewrite act) (emptyRewriterInfo e)
    return (b, _riBlockRecoveryDiagnostics ri)
  where
    act = do
      symmap <- withCurrentELF (buildSymbolMap mem)
      doAnalysis cfg mem symmap

withElf :: E.SomeElf E.Elf -> (forall w . E.Elf w -> a) -> a
withElf e k =
  case e of
    E.Elf32 e32 -> k e32
    E.Elf64 e64 -> k e64

-- | Extract the 'MM.Memory' from an ELF file.
withMemory :: forall w m a
            . (C.MonadThrow m, MM.MemWidth w, Integral (E.ElfWordType w))
           => E.Elf w
           -> (MM.Memory w -> m a)
           -> m a
withMemory e k =
  case MM.memoryForElf loadOpts e of
    Left err -> C.throwM (MemoryLoadError err)
    Right (_sim, mem) -> k mem
  where
    loadOpts = MM.defaultLoadOptions { MM.loadRegionIndex = Just 0 }

findTextSection :: E.Elf w -> ElfRewriter w (E.ElfSection (E.ElfWordType w))
findTextSection e = do
  case E.findSectionByName (C8.pack ".text") e of
    [textSection] -> return textSection
    [] -> C.throwM NoTextSectionFound
    sections -> C.throwM (MultipleTextSectionsFound (length sections))

-- | Call the given continuation with the current ELF file
--
-- The intention is that functions that simply *read* the current ELF file are
-- wrapped in this combinator as a marker that they are read-only.  Functions
-- that modify the current ELF file will be wrapped in 'modifyCurrentELF'.
withCurrentELF :: (E.Elf w -> ElfRewriter w a) -> ElfRewriter w a
withCurrentELF k = do
  elf <- S.gets _riELF
  k elf

-- | A wrapper around functions that modify the current ELF file.
--
-- The modification function can return some extra data if desired.  The idea is that this is a signal that
-- a function mutates the ELF file.
--
-- This should be the only function that writes to the current ELF file
modifyCurrentELF :: (E.Elf w -> ElfRewriter w (a, E.Elf w)) -> ElfRewriter w a
modifyCurrentELF k = do
  elf <- S.gets _riELF
  (res, elf') <- k elf
  S.modify' $ \s -> s { _riELF = elf' }
  return res


-- | The rewriter driver
doRewrite :: (ISA.InstructionConstraints i a,
              Typeable w,
              E.ElfWidthConstraints w,
              KnownNat w,
              R.ArchBits arch w)
          => RenovateConfig i a w arch b
          -> MM.Memory w
          -> RM.SymbolMap w
          -> RE.LayoutStrategy
          -> ElfRewriter w b
doRewrite cfg mem symmap strat = do
  -- We pull some information from the unmodified initial binary: the text
  -- section, the entry point(s), and original symbol table (if any).
  textSection <- withCurrentELF findTextSection
  -- (entryPoint, _otherEntries) <- withCurrentELF (entryPoints mem)
  mBaseSymtab <- withCurrentELF getBaseSymbolTable

  -- Remove (and pad out) the sections whose size could change if we
  -- modify the binary.  We'll re-add them later (see @appendSegment@).
  --
  -- This modifies the underlying ELF file
  modifyCurrentELF padDynamicDataRegions

  -- We need to compute our instrumentation address *after* we have
  -- removed all of the possibly dynamic sections and ensured that
  -- everything will line up.
  nextSegmentAddress <- withCurrentELF (segmentLayoutAddress (rcCodeLayoutBase cfg))
--  traceM $ printf "Extra text section layout address is 0x%x" (fromIntegral nextSegmentAddress :: Word64)
  riSegmentVirtualAddress L..= Just (fromIntegral nextSegmentAddress)


  -- Perform the brittle transformation
  --
  -- This computes the new contents of the .text section
  -- (overwrittenBytes) and the contents of the new code segment
  -- (instrumentedBytes), which will be placed at the address computed
  -- above.
  let layoutAddr = RA.concreteFromAbsolute (fromIntegral nextSegmentAddress)
      -- FIXME: This is wrong; it doesn't account for the required alignment we
      -- need.  That is a big challenge because it depends on how much code we
      -- generate.  Maybe we can do something with congruence where we waste up
      -- to a page of space to maintain alignment.
      dataAddr = RA.concreteFromAbsolute (fromIntegral (rcDataLayoutBase cfg))
      textSectionStartAddr = RA.concreteFromAbsolute (fromIntegral (E.elfSectionAddr textSection))
      textSectionEndAddr = RA.addressAddOffset textSectionStartAddr (fromIntegral ((E.elfSectionSize textSection)))

  ( analysisResult
    , overwrittenBytes
    , instrumentedBytes
    , mNewData
    , newSyms ) <- instrumentTextSection cfg mem textSectionStartAddr textSectionEndAddr
                                       (E.elfSectionData textSection) strat layoutAddr dataAddr symmap

  (extraTextSecIdx, instrumentationSeg) <- withCurrentELF (newInstrumentationSegment nextSegmentAddress instrumentedBytes)

  riOriginalTextSize L..= fromIntegral (B.length overwrittenBytes)
  riNewTextSize L..= fromIntegral (B.length instrumentedBytes)

  -- Now go through and append our new segments.  The first contains our
  -- instrumentation.  The second contains a new data segment (if required).
  -- The third includes the headers that we had to remove initially.
  --
  -- Note that @appendSegment@ does a bit of work to ensure proper
  -- alignment.
  modifyCurrentELF (appendSegment instrumentationSeg)
  case mNewData of
    Nothing -> return ()
    Just newData -> do
      dataSegAddr <- withCurrentELF (segmentLayoutAddress (rcDataLayoutBase cfg))
      newDataSeg <- withCurrentELF (newDataSegment dataSegAddr newData)
      modifyCurrentELF (appendSegment newDataSeg)
  baseAddr            <- withCurrentELF findBaseAddr
  newProgramHeaderSeg <- withCurrentELF (newProgramHeaderSegment baseAddr)
  modifyCurrentELF (appendSegment newProgramHeaderSeg)
  case mBaseSymtab of
    Just baseSymtab
      | rcUpdateSymbolTable cfg -> do
          newSymtab <- withCurrentELF (buildNewSymbolTable (E.elfSectionIndex textSection) extraTextSecIdx layoutAddr newSyms baseSymtab)
          modifyCurrentELF (appendDataRegion (E.ElfDataSymtab newSymtab))
    _ -> return ()
  modifyCurrentELF appendHeaders

  -- Now overwrite the original code (in the .text segment) with the
  -- content computed by our transformation.
  modifyCurrentELF (overwriteTextSection overwrittenBytes)
  return analysisResult

-- | The analysis driver
doAnalysis :: (ISA.InstructionConstraints i a,
               Typeable w,
               E.ElfWidthConstraints w,
               KnownNat w,
               R.ArchBits arch w)
           => RenovateConfig i a w arch b
           -> MM.Memory w
           -> RM.SymbolMap w
           -> ElfRewriter w b
doAnalysis cfg mem symmap = do
--  (entryPoint, _otherEntries) <- withCurrentELF (entryPoints mem)

  -- We need to compute our instrumentation address *after* we have
  -- removed all of the possibly dynamic sections and ensured that
  -- everything will line up.
  nextSegmentAddress <- withCurrentELF (segmentLayoutAddress (rcCodeLayoutBase cfg))
--  traceM $ printf "Extra text section layout address is 0x%x" (fromIntegral nextSegmentAddress :: Word64)
  riSegmentVirtualAddress L..= Just (fromIntegral nextSegmentAddress)

  analysisResult <- analyzeTextSection cfg mem symmap
  return analysisResult

buildSymbolMap :: Integral (E.ElfWordType w)
               => MM.MemWidth w
               => MM.Memory w
               -> E.Elf w
               -> ElfRewriter w (RM.SymbolMap w)
buildSymbolMap _mem elf = do
  case filter isSymbolTable (F.toList (E._elfFileData elf)) of
    [E.ElfDataSymtab table] -> do
      let entries = catMaybes (map mkPair (F.toList (E.elfSymbolTableEntries table)))
      return (foldr (uncurry Map.insert) mempty entries)
    -- TODO: can there be more than 1 symbol table?
    _ -> return mempty
  where
    mkPair e
      | let addr = RA.concreteFromAbsolute (fromIntegral (E.steValue e))
      , E.steType e == E.STT_FUNC = Just (addr, E.steName e)
      | otherwise = Nothing

isSymbolTable :: E.ElfDataRegion w -> Bool
isSymbolTable (E.ElfDataSymtab{}) = True
isSymbolTable _                   = False

buildNewSymbolTable :: (E.ElfWidthConstraints w, MM.MemWidth w)
                    => Word16
                    -> E.ElfSectionIndex
                    -> RA.ConcreteAddress w
                    -> RM.NewSymbolsMap w
                    -> E.ElfSymbolTable (E.ElfWordType w)
                    -- ^ The original symbol table
                    -> E.Elf w
                    -> ElfRewriter w (E.ElfSymbolTable (E.ElfWordType w))
buildNewSymbolTable textSecIdx extraTextSecIdx layoutAddr newSyms baseTable elf
  | trace (printf "New symbol table index is %d" (nextSectionIndex elf)) False = undefined
  | otherwise =
  return $ baseTable { E.elfSymbolTableEntries = E.elfSymbolTableEntries baseTable <>
                       newEntries (toMap (E.elfSymbolTableEntries baseTable))
                     , E.elfSymbolTableIndex = nextSectionIndex elf
                     }
  where
    toMap      t = Map.fromList [ (E.steValue e, e) | e <- V.toList t ]
    newEntries t = V.fromList   [ newFromEntry textSecIdx extraTextSecIdx layoutAddr e ca nm
                                | (ca, (oa, nm)) <- Map.toList newSyms
                                , e <- maybeToList $! Map.lookup (fromIntegral (RA.absoluteAddress oa)) t
                                ]

-- | Get the current symbol table
getBaseSymbolTable :: E.Elf w
                   -> ElfRewriter w (Maybe (E.ElfSymbolTable (E.ElfWordType w)))
getBaseSymbolTable = return . listToMaybe . E.elfSymtab

newFromEntry :: MM.MemWidth w
             => E.ElfWidthConstraints w
             => Word16
             -> E.ElfSectionIndex
             -> RA.ConcreteAddress w
             -> E.ElfSymbolTableEntry (E.ElfWordType w)
             -> RA.ConcreteAddress w
             -> B.ByteString
             -> E.ElfSymbolTableEntry (E.ElfWordType w)
newFromEntry textSecIdx extraTextSecIdx layoutAddr e addr nm = e
  { E.steName  = "__embrittled_" `B.append` nm
  , E.steValue = fromIntegral absAddr
  , E.steIndex = if absAddr >= RA.absoluteAddress layoutAddr
                 then extraTextSecIdx
                 else E.ElfSectionIndex textSecIdx
  }
  where
    absAddr = RA.absoluteAddress addr

-- | Create a new data segment containing a single data section, containing the given bytestring
--
-- It will be placed at the given start address.
newDataSegment :: (Num (E.ElfWordType w), Show (E.ElfWordType w), Integral (E.ElfWordType w), Bits (E.ElfWordType w))
               => E.ElfWordType w
               -> B.ByteString
               -> E.Elf w
               -> ElfRewriter w (E.ElfSegment w)
newDataSegment startAddr bytes e = do
  let sec = E.ElfSection { E.elfSectionName = C8.pack "brittle_data"
                       , E.elfSectionType = E.SHT_PROGBITS
                       , E.elfSectionFlags = E.shf_alloc .|. E.shf_write
                       , E.elfSectionAddr = startAddr
                       , E.elfSectionSize = fromIntegral (B.length bytes)
                       , E.elfSectionLink = 0
                       , E.elfSectionInfo = 0
                       , E.elfSectionAddrAlign = 1
                       , E.elfSectionEntSize = 0
                       , E.elfSectionData = bytes
                       , E.elfSectionIndex = nextSectionIndex e `debug` ("New data segment index: " ++ show (nextSectionIndex e))
                       }
  let seg = E.ElfSegment { E.elfSegmentType = E.PT_LOAD
                       , E.elfSegmentFlags = E.pf_r .|. E.pf_w
                       , E.elfSegmentIndex = nextSegmentIndex e
                       , E.elfSegmentVirtAddr = startAddr
                       , E.elfSegmentPhysAddr = startAddr
                       , E.elfSegmentAlign = 0x200000
                       , E.elfSegmentMemSize = E.ElfRelativeSize 0
                       , E.elfSegmentData = Seq.singleton (E.ElfDataSection sec)
                       }
  return seg

-- | We store the program headers in there own segment so that the C runtime can
-- look at them during libc init. Some padding is needed to get the alignment to
-- work out. The address needs to be the start address for the elf. This can
-- usually be calculated with `findBaseAddr`.
newProgramHeaderSegment :: (Num (E.ElfWordType w), Show (E.ElfWordType w), Integral (E.ElfWordType w), Bits (E.ElfWordType w))
                        => E.ElfWordType w
                        -> E.Elf w
                        -> ElfRewriter w (E.ElfSegment w)
newProgramHeaderSegment baseAddr e = do
  let layout        = E.elfLayout e
      sz            = E.elfLayoutSize layout
      alignedOffset = fixAlignment sz (fromIntegral pageAlignment)
  let seg = E.ElfSegment
            { E.elfSegmentType     = E.PT_LOAD
            , E.elfSegmentFlags    = E.pf_r
            , E.elfSegmentIndex    = nextSegmentIndex e
            , E.elfSegmentVirtAddr = baseAddr + alignedOffset
            , E.elfSegmentPhysAddr = baseAddr + alignedOffset
            , E.elfSegmentAlign    = fromIntegral pageAlignment
            , E.elfSegmentMemSize  = E.ElfRelativeSize 0
            , E.elfSegmentData     = E.ElfDataSegmentHeaders Seq.<| Seq.empty
            }
  return seg

-- | Finds the lowest address mentioned in a "LOAD"able elf segment. We treat
-- this as the base address that the elf file will be loaded at.
--
-- Note: This assumes there is at least one PT_LOAD segment, which should
-- normally be the case.
findBaseAddr :: forall w
              . (Num (E.ElfWordType w), Show (E.ElfWordType w), Integral (E.ElfWordType w), Bits (E.ElfWordType w))
             => E.Elf w
             -> ElfRewriter w (E.ElfWordType w)
findBaseAddr e = do
  let segs :: [ E.ElfDataRegion w ]
      segs = e L.^. E.elfFileData . L.to F.toList
      addrs = [ E.elfSegmentVirtAddr s
              | E.ElfDataSegment s <- segs
              , E.elfSegmentType s == E.PT_LOAD ]
  return $! minimum addrs

-- | Replace all of the dynamically-sized data regions in the ELF file with padding.
--
-- By dynamically-sized, we mean sections whose sizes will change if
-- we add a new section or segment.  These sections are the section
-- and segment tables, the section name table, and the symbol table.
--
-- We replace them with padding so that none of the original contents
-- of the binary need to move.  We will re-create these sections at
-- the end of the binary when we have finished rewriting it.
padDynamicDataRegions :: Integral (E.ElfWordType w) => E.Elf w -> ElfRewriter w ((), E.Elf w)
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

appendDataRegion :: (Ord (E.ElfWordType w), Integral (E.ElfWordType w))
                 => E.ElfDataRegion w
                 -> E.Elf w
                 -> ElfRewriter w ((), E.Elf w)
appendDataRegion r e = do
  let layout = E.elfLayout e
      sz = E.elfLayoutSize layout
      alignedOffset = fixAlignment sz (fromIntegral pageAlignment)
      paddingBytes = alignedOffset - sz
      paddingRegion = E.ElfDataRaw (B.replicate (fromIntegral paddingBytes) 0)
  let dats = if paddingBytes > 0 then [ paddingRegion, r ] else [ r ]
  return ((), e L.& E.elfFileData L.%~ (`mappend` Seq.fromList dats))



-- | Append a segment to the given ELF file, adding the necessary
-- padding before with an ElfDataRaw data region.
appendSegment :: Ord (E.ElfWordType w)
              => Integral (E.ElfWordType w)
              => E.ElfSegment w
              -> E.Elf w
              -> ElfRewriter w ((), E.Elf w)
appendSegment seg e = do
  let layout = E.elfLayout e
      sz = E.elfLayoutSize layout
      alignedOffset = fixAlignment sz (fromIntegral pageAlignment)
      paddingBytes = alignedOffset - sz
      paddingRegion = E.ElfDataRaw (B.replicate (fromIntegral paddingBytes) 0)
  riAppendedSegments L.%= ((E.elfSegmentType seg,
                            E.elfSegmentIndex seg,
                            fromIntegral alignedOffset,
                            fromIntegral paddingBytes):)
  let dats = if paddingBytes > 0 then [ paddingRegion, E.ElfDataSegment seg ] else [ E.ElfDataSegment seg ]
  return ((), e L.& E.elfFileData L.%~ (`mappend` Seq.fromList dats))

-- | Append the necessary program header data onto the end of the ELF file.
--
-- This includes: section headers, segment headers, and the section name table.
appendHeaders :: (Show (E.ElfWordType w), Bits (E.ElfWordType w), Integral (E.ElfWordType w))
              => E.Elf w
              -> ElfRewriter w ((), E.Elf w)
appendHeaders elf = do
  let shstrtabidx = nextSectionIndex elf
  let strtabidx = shstrtabidx + 1
--  traceM $ printf "shstrtabidx = %d" shstrtabidx
  let elfData = [ E.ElfDataSectionHeaders
                , E.ElfDataSectionNameTable shstrtabidx
                , E.ElfDataStrtab strtabidx
                ]
  return ((), elf L.& E.elfFileData L.%~ (`mappend` Seq.fromList elfData))

nextSectionIndex :: (Bits (E.ElfWordType s), Show (E.ElfWordType s), Integral (E.ElfWordType s)) => E.Elf s -> Word16
nextSectionIndex e = firstAvailable 0 indexes
  where
    indexes  = Map.keys (E.elfLayout e L.^. E.shdrs)

    firstAvailable ix [] = ix
    firstAvailable ix (next:rest)
      | ix == next = firstAvailable (ix + 1) rest
      | otherwise = ix

nextSegmentIndex :: E.Elf w -> Word16
nextSegmentIndex = fromIntegral . length . E.elfSegments

replaceSectionWithPadding :: Integral (E.ElfWordType w)
                          => E.ElfLayout w
                          -> (E.ElfDataRegion w -> Bool)
                          -> E.ElfDataRegion w
                          -> ElfRewriter w (E.ElfDataRegion w)
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

-- | Find a location that we can put our instrumented code at.
--
-- The key is that we need an address for which we can meet the
-- alignment congruence condition.  We do that by construction.  We
-- choose a base address for the code (the @instrumentationBase@
-- constant defined at the top of the file) and then add in enough
-- padding to the end of the file to bump the address up to the next
-- page boundary.
--
-- When we assemble the final binary, neither the offset nor the
-- virtual address for the segment will be divisible by the executable
-- alignment (0x20000), but they will be congruent (i.e., have the
-- same remainder).
segmentLayoutAddress :: (Num (E.ElfWordType w), Integral (E.ElfWordType w))
                     => Word64
                     -> E.Elf w
                     -> ElfRewriter w (E.ElfWordType w)
segmentLayoutAddress segBaseAddr e = do
  let layout = E.elfLayout e
  let totalSize = F.sum $ fmap (E.elfRegionFileSize layout) (L.view E.elfFileData e)
  let addr :: Int
      addr = fromIntegral totalSize
  let aligned = fixAlignment addr (fromIntegral pageAlignment)
  return (fromIntegral segBaseAddr + fromIntegral aligned)

fixAlignment :: Integral w => w -> w -> w
fixAlignment v 0 = v
fixAlignment v 1 = v
fixAlignment v a0
  | m == 0 = c * a
  | otherwise = (c + 1) * a
  where
    a = fromIntegral a0
    (c,m) = v `divMod` a

overwriteTextSection :: (Integral (E.ElfWordType w)) => B.ByteString -> E.Elf w -> ElfRewriter w ((), E.Elf w)
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

newInstrumentationSegment :: (Num (E.ElfWordType w), Show (E.ElfWordType w), Integral (E.ElfWordType w), Bits (E.ElfWordType w))
                          => Bits (E.ElfWordType w)
                          => E.ElfWordType w
                          -> B.ByteString
                          -> E.Elf w
                          -> ElfRewriter w (E.ElfSectionIndex, E.ElfSegment w)
newInstrumentationSegment startAddr bytes e = do
  let txtIdx = nextSectionIndex e
--  traceM ("New text section index: " ++ show txtIdx)
  let sec = E.ElfSection { E.elfSectionName = C8.pack "brittle"
                       , E.elfSectionType = E.SHT_PROGBITS
                       , E.elfSectionFlags = E.shf_alloc .|. E.shf_execinstr
                       , E.elfSectionAddr = startAddr
                       , E.elfSectionSize = fromIntegral (B.length bytes)
                       , E.elfSectionLink = 0
                       , E.elfSectionInfo = 0
                       , E.elfSectionAddrAlign = 1
                       , E.elfSectionEntSize = 0
                       , E.elfSectionData = bytes
                       , E.elfSectionIndex = txtIdx
                       }
  let seg = E.ElfSegment { E.elfSegmentType = E.PT_LOAD
                       , E.elfSegmentFlags = E.pf_r .|. E.pf_x
                       , E.elfSegmentIndex = nextSegmentIndex e
                       , E.elfSegmentVirtAddr = startAddr
                       , E.elfSegmentPhysAddr = startAddr
                       , E.elfSegmentAlign = 0x200000
                       , E.elfSegmentMemSize = E.ElfRelativeSize 0
                       , E.elfSegmentData = Seq.singleton (E.ElfDataSection sec)
                       }
  return (E.ElfSectionIndex txtIdx, seg)

-- | Architecture-specific entry point identification
--
-- Most architectures have a simple approach: take the entry point named in the
-- ELF file (and any entry points identified by symbols) and just use those.
-- PowerPC has special treatment because the ELF entry point value actually
-- points to the address of the Table of Contents (TOC) entry for the entry
-- point.
findEntryPoints :: (MM.MemWidth w, Integral (E.ElfWordType w))
                => RenovateConfig i a w arch b
                -> E.Elf w
                -> MM.Memory w
                -> NEL.NonEmpty (MM.MemSegmentOff w)
findEntryPoints cfg elf mem =
  case E.elfMachine elf of
    E.EM_PPC64 ->
      let tocEntryAddr = E.elfEntry elf
          Right addr = MM.readAddr mem MM.BigEndian (MM.absoluteAddr (MM.memWord (fromIntegral tocEntryAddr)))
          Just entryPoint = MM.asSegmentOff mem addr
      in entryPoint NEL.:| mapMaybe (MM.asSegmentOff mem) (rcELFEntryPoints cfg elf)
    E.EM_PPC ->
      let tocEntryAddr = E.elfEntry elf
          Right addr = MM.readAddr mem MM.BigEndian (MM.absoluteAddr (MM.memWord (fromIntegral tocEntryAddr)))
          Just entryPoint = MM.asSegmentOff mem addr
      in entryPoint NEL.:| mapMaybe (MM.asSegmentOff mem) (rcELFEntryPoints cfg elf)
    _ ->
      let Just entryPoint = MM.asSegmentOff mem (MM.absoluteAddr (MM.memWord (fromIntegral (E.elfEntry elf))))
      in entryPoint NEL.:| mapMaybe (MM.asSegmentOff mem) (rcELFEntryPoints cfg elf)


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
instrumentTextSection :: forall i a w arch b
                       . (ISA.InstructionConstraints i a,
                          Typeable w,
                          KnownNat w,
                          Integral (E.ElfWordType w),
                          R.ArchBits arch w)
                      => RenovateConfig i a w arch b
                      -> MM.Memory w
                      -- ^ The memory space
                      -> RA.ConcreteAddress w
                      -- ^ The address of the start of the text section
                      -> RA.ConcreteAddress w
                      -- ^ The address of the end of the text section
                      -> B.ByteString
                      -- ^ The bytes of the text section
                      -> RE.LayoutStrategy
                      -- ^ The strategy to use for laying out instrumented blocks
                      -> RA.ConcreteAddress w
                      -- ^ The address to lay out the instrumented blocks
                      -> RA.ConcreteAddress w
                      -- ^ The address to lay out the new data section
                      -> RM.SymbolMap w
                      -- ^ meta data?
                      -> ElfRewriter w (b, B.ByteString, B.ByteString, Maybe B.ByteString, RM.NewSymbolsMap w)
instrumentTextSection cfg mem textSectionStartAddr textSectionEndAddr textBytes strat layoutAddr newGlobalBase symmap = do
  -- We use an irrefutable match on the entry point -- we are asserting that the
  -- entry point is mapped in the 'MM.Memory' object passed in.
--  let Just entrySegOff = RA.concreteAsSegmentOff mem entryPoint
--  traceM ("instrumentTextSection entry point: " ++ show entryPoint)
--  riEntryPointAddress L..= (fromIntegral <$> MM.msegAddr entrySegOff)
  elfEntryPoints@(entryPoint NEL.:| _) <- withCurrentELF $ \e -> return (findEntryPoints cfg e mem)
  let isa = rcISA cfg
      archInfo = rcArchInfo cfg
  recRes <- IO.liftIO (R.recoverBlocks (rcBlockCallback cfg) (rcFunctionCallback cfg) isa (rcDisassembler cfg) archInfo mem symmap elfEntryPoints)
  case recRes of
    Left exn1 -> do
      riBlockRecoveryDiagnostics L..= []
      C.throwM (BlockRecoveryFailure exn1 [])
    Right blockInfo -> do
--      traceM "Recovered blocks with macaw"
      riBlockRecoveryDiagnostics L..= []
      let blocks = R.biBlocks blockInfo
      riRecoveredBlocks L..= Just (SomeBlocks (rcISA cfg) blocks)
      let cfgs = FR.recoverFunctions isa mem blockInfo
      case cfg of
        -- This pattern match is only here to deal with the existential
        -- quantification inside of RenovateConfig.
        RenovateConfig { rcAnalysis = analysis, rcRewriter = rewriter } ->
          let analysisResult = analysis isa mem blockInfo
              Just concEntryPoint = RA.concreteFromSegmentOff mem entryPoint
          in
          case RW.runRewriteM mem
                              concEntryPoint
                              newGlobalBase
                              cfgs
                              (RE.redirect isa textSectionStartAddr textSectionEndAddr (rewriter analysisResult isa mem) mem strat layoutAddr blocks symmap)
          of
            (rres, info) ->
              case RE.checkRedirection rres of
                Left exn2 -> do
                  riRedirectionDiagnostics L..= RE.rdDiagnostics rres
                  C.throwM (RewriterFailure exn2 (RE.rdDiagnostics rres))
                Right redir -> do
                  let I.Identity (overwrittenBlocks, instrumentationBlocks) = RE.rdBlocks redir
                  let newSyms = RE.rdNewSymbols redir
                  riRedirectionDiagnostics L..= RE.rdDiagnostics rres
                  riInstrumentationSites L..= RW.infoSites info
                  riReusedByteCount L..= RE.rdReusedBytes redir
                  riSmallBlockCount L..= RE.rdSmallBlock redir
                  riUnrelocatableTerm L..= RE.rdUnrelocatableTerm redir
                  riBlockMapping L..= RE.rdBlockMapping redir
                  let allBlocks = overwrittenBlocks ++ instrumentationBlocks
                  case cfg of
                    RenovateConfig { rcAssembler = asm } -> do
                      (overwrittenBytes, instrumentationBytes) <- BA.assembleBlocks mem isa textSectionStartAddr textSectionEndAddr textBytes layoutAddr asm allBlocks
                      let newDataBytes = mkNewDataSection newGlobalBase info
                      return (analysisResult, overwrittenBytes, instrumentationBytes, newDataBytes, newSyms)

analyzeTextSection :: forall i a w arch b
                    . (ISA.InstructionConstraints i a,
                       Typeable w,
                       KnownNat w,
                       Integral (E.ElfWordType w),
                       R.ArchBits arch w)
                   => RenovateConfig i a w arch b
                   -> MM.Memory w
                   -- ^ The memory space
                   -> RM.SymbolMap w
                   -> ElfRewriter w b
analyzeTextSection cfg mem symmap = do
  elfEntryPoints <- withCurrentELF $ \e -> return (findEntryPoints cfg e mem)
--  traceM ("analyzeTextSection entry point: " ++ show entryPoint)
--  riEntryPointAddress L..= (fromIntegral <$> MM.msegAddr entryPoint)
  let isa      = rcISA cfg
      archInfo = rcArchInfo cfg
  recRes <- IO.liftIO (R.recoverBlocks (rcBlockCallback cfg) (rcFunctionCallback cfg) isa (rcDisassembler cfg) archInfo mem symmap elfEntryPoints)
  case recRes of -- R.recoverBlocks isa (rcDisassembler cfg) archInfo mem elfEntryPoints of
    Left exn1 -> do
      riBlockRecoveryDiagnostics L..= []
      C.throwM (BlockRecoveryFailure exn1 [])
    Right blockInfo -> do
--      traceM "Recovered blocks with macaw"
      riBlockRecoveryDiagnostics L..= []
      let blocks = R.biBlocks blockInfo
      riRecoveredBlocks L..= Just (SomeBlocks (rcISA cfg) blocks)
      return $! (rcAnalysis cfg) isa mem blockInfo

mkNewDataSection :: (MM.MemWidth w) => RA.ConcreteAddress w -> RW.RewriteInfo w -> Maybe B.ByteString
mkNewDataSection baseAddr info = do
  guard (bytes > 0)
  return (B.pack (replicate bytes 0))
  where
    bytes = fromIntegral (RW.nextGlobalAddress info `RA.addressDiff` baseAddr)

data ElfRewriteException = RewrittenTextSectionSizeMismatch Int Int
                         | StringTableNotFound
                         | BlockRecoveryFailure C.SomeException [RD.Diagnostic]
                         | RewriterFailure C.SomeException [RD.Diagnostic]
                         | UnsupportedArchitecture E.ElfMachine
                         | MemoryLoadError String
                         | NoTextSectionFound
                         | MultipleTextSectionsFound Int
                         deriving (Typeable)

deriving instance Show ElfRewriteException
instance C.Exception ElfRewriteException

newtype ElfRewriter w a = ElfRewriter { unElfRewrite :: S.StateT (RewriterInfo w) IO a }
                          deriving (Monad,
                                    Functor,
                                    Applicative,
                                    IO.MonadIO,
                                    P.MonadThrow,
                                    S.MonadState (RewriterInfo w))

emptyRewriterInfo :: E.Elf w -> RewriterInfo w
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
                                   , _riNewTextSize              = 0
                                   , _riBlockMapping             = []
                                   }
riOriginalTextSize :: L.Simple L.Lens (RewriterInfo w) Int
riOriginalTextSize = GL.field @"_riOriginalTextSize"

riNewTextSize :: L.Simple L.Lens (RewriterInfo w) Int
riNewTextSize = GL.field @"_riNewTextSize"

riSegmentVirtualAddress :: L.Simple L.Lens (RewriterInfo w) (Maybe Word64)
riSegmentVirtualAddress = GL.field @"_riSegmentVirtualAddress"

riOverwrittenRegions :: L.Simple L.Lens (RewriterInfo w) [(String, Word64)]
riOverwrittenRegions = GL.field @"_riOverwrittenRegions"

riAppendedSegments :: L.Simple L.Lens (RewriterInfo w) [(E.ElfSegmentType, Word16, Word64, Word64)]
riAppendedSegments = GL.field @"_riAppendedSegments"

riEntryPointAddress :: L.Simple L.Lens (RewriterInfo w) (Maybe Word64)
riEntryPointAddress = GL.field @"_riEntryPointAddress"

riSectionBaseAddress :: L.Simple L.Lens (RewriterInfo w) (Maybe Word64)
riSectionBaseAddress = GL.field @"_riSectionBaseAddress"

riInitialBytes :: L.Simple L.Lens (RewriterInfo w) (Maybe B.ByteString)
riInitialBytes = GL.field @"_riInitialBytes"

riBlockRecoveryDiagnostics :: L.Simple L.Lens (RewriterInfo w) [RD.Diagnostic]
riBlockRecoveryDiagnostics = GL.field @"_riBlockRecoveryDiagnostics"

riRedirectionDiagnostics :: L.Simple L.Lens (RewriterInfo w) [RD.Diagnostic]
riRedirectionDiagnostics = GL.field @"_riRedirectionDiagnostics"

riRecoveredBlocks :: L.Simple L.Lens (RewriterInfo w) (Maybe SomeBlocks)
riRecoveredBlocks = GL.field @"_riRecoveredBlocks"

riInstrumentationSites :: L.Simple L.Lens (RewriterInfo w) [RW.RewriteSite w]
riInstrumentationSites = GL.field @"_riInstrumentationSites"

riSmallBlockCount :: L.Simple L.Lens (RewriterInfo w) Int
riSmallBlockCount = GL.field @"_riSmallBlockCount"

riReusedByteCount :: L.Simple L.Lens (RewriterInfo w) Int
riReusedByteCount = GL.field @"_riReusedByteCount"

riUnrelocatableTerm :: L.Simple L.Lens (RewriterInfo w) Int
riUnrelocatableTerm = GL.field @"_riUnrelocatableTerm"

riBlockMapping :: L.Simple L.Lens (RewriterInfo w) [(RA.ConcreteAddress w, RA.ConcreteAddress w)]
riBlockMapping = GL.field @"_riBlockMapping"
