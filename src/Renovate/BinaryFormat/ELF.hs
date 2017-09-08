{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
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
  entryPoints,
  riSectionBaseAddress,
  riInitialBytes,
  Rewriter(..),
  RenovateConfig(..),
  RewriterInfo(..),
  SomeBlocks(..)
  ) where

import           GHC.TypeLits ( KnownNat )

import           Control.Applicative
import qualified Control.Lens as L
import           Control.Monad ( guard, when )
import qualified Control.Monad.Catch as C
import qualified Control.Monad.Catch.Pure as P
import qualified Control.Monad.State.Strict as S
import           Data.Bits ( Bits, (.|.) )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as Map
import           Data.Maybe ( catMaybes, maybeToList, listToMaybe )
import qualified Data.Vector as V
import           Data.Monoid
import qualified Data.Sequence as Seq
import           Data.Typeable ( Typeable )
import           Data.Word ( Word16, Word32, Word64 )
import           Text.Printf ( printf )

import           Prelude

import qualified Data.ElfEdit as E
import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Memory.ElfLoader as MM
import qualified Data.Macaw.Types as MM

import qualified Renovate.Address as SFE
import qualified Renovate.Analysis.FunctionRecovery as FR
import qualified Renovate.Arch.X86_64 as X86_64
import qualified Renovate.BasicBlock.Assemble as SFE
import qualified Renovate.ISA as ISA
import qualified Renovate.Recovery as R
import qualified Renovate.Redirect as SFE
import qualified Renovate.Redirect.Monad as SFEM
import           Renovate.Config

import qualified Renovate.Instrument as I
import           Debug.Trace
debug :: a -> String -> a
debug = flip trace
-- | The system page alignment (assuming 4k pages)
pageAlignment :: Word32
pageAlignment = 0x1000

-- | The first address we would place our instrumentation at.
--
-- This is arbitrary (and not necessarily always correct, if there is
-- a huge data section).  The normal .text section is at 0x400000,
-- while data is at 0x600000.
instrumentationBase :: Word32
instrumentationBase = 0x800000

newDataSectionBase :: Word32
newDataSectionBase  = 0xa00000

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
               , _riBlockRecoveryDiagnostics :: [R.Diagnostic]
               , _riRedirectionDiagnostics :: [SFE.Diagnostic]
               , _riRecoveredBlocks :: Maybe SomeBlocks
               , _riInstrumentationInfo :: Maybe (I.InstrumentInfo w)
               , _riELF :: E.Elf w
               }

data SomeBlocks = forall i a w
                . (MM.MemWidth w, ISA.InstructionConstraints i a)
                => SomeBlocks (ISA.ISA i a w) [SFE.ConcreteBlock i w]

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
              -> Rewriter
              -> (forall i a w arch . (R.ArchBits arch w,
                                       Typeable w,
                                       KnownNat w,
                                       E.ElfWidthConstraints w,
                                       ISA.InstructionConstraints i a)
                                   => RenovateConfig i a w arch
                                   -> E.Elf w
                                   -> MM.Memory w
                                   -> m t)
              -> m t
withElfConfig e0 i k =
  case (e0, withElf e0 E.elfMachine) of
    (E.Elf32 _, mach) ->
      -- No support for 32 bit architectures yet.  Should change with ARM
      C.throwM (UnsupportedArchitecture mach)
    (E.Elf64 e, E.EM_X86_64) ->
      withMemory e $ k X86_64.config { rcInstrumentor = iX86_64 i } e
    (E.Elf64 _, mach) -> C.throwM (UnsupportedArchitecture mach)

rewriteElf :: (ISA.InstructionConstraints i a,
               E.ElfWidthConstraints w,
               KnownNat w,
               Typeable w,
               R.ArchBits arch w)
           => RenovateConfig i a w arch
           -> E.Elf w
           -> MM.Memory w
           -> SFE.LayoutStrategy
           -> Either C.SomeException (E.Elf w, RewriterInfo w)
rewriteElf cfg e mem strat =
  P.runCatch $ do
    ri <- S.execStateT (unElfRewrite act) (emptyRewriterInfo e)
    return (_riELF ri, ri)
  where
    act = doRewrite cfg mem strat

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
  case MM.memoryForElf (MM.LoadOptions MM.LoadBySection False) e of
    Left err -> C.throwM (MemoryLoadError err)
    Right (_sim, mem) -> k mem

-- | Look up the entry point(s) for an ELF file.
--
-- There is always at least one entry point named by the ELF file
-- (usually the start instruction).  This function can also return
-- other entry points (e.g., symbols mentioned in the symbol table).
entryPoints :: (MM.MemWidth w, Integral (E.ElfWordType w))
            => MM.Memory w
            -> E.Elf w
            -> ElfRewriter w (MM.MemSegmentOff w, [MM.MemSegmentOff w])
entryPoints mem elf = do
  let Just entryPoint = MM.resolveAbsoluteAddr mem (fromIntegral (E.elfEntry elf))
  return (entryPoint, [])

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
          => RenovateConfig i a w arch
          -> MM.Memory w
          -> SFE.LayoutStrategy
          -> ElfRewriter w ()
doRewrite cfg mem strat = do
  -- We pull some information from the unmodified initial binary: the text
  -- section, the entry point(s), and original symbol table (if any).
  textSection <- withCurrentELF findTextSection
  (entryPoint, _otherEntries) <- withCurrentELF (entryPoints mem)
  symmap <- withCurrentELF (buildSymbolMap mem)
  mBaseSymtab <- withCurrentELF getBaseSymbolTable

  -- Remove (and pad out) the sections whose size could change if we
  -- modify the binary.  We'll re-add them later (see @appendSegment@).
  --
  -- This modifies the underlying ELF file
  modifyCurrentELF padDynamicDataRegions

  -- We need to compute our instrumentation address *after* we have
  -- removed all of the possibly dynamic sections and ensured that
  -- everything will line up.
  nextSegmentAddress <- withCurrentELF (segmentLayoutAddress instrumentationBase)
  traceM $ printf "Extra text section layout address is 0x%x" (fromIntegral nextSegmentAddress :: Word64)
  riSegmentVirtualAddress L..= Just (fromIntegral nextSegmentAddress)


  -- Perform the brittle transformation
  --
  -- This computes the new contents of the .text section
  -- (overwrittenBytes) and the contents of the new code segment
  -- (instrumentedBytes), which will be placed at the address computed
  -- above.
  let -- FIXME: Find a real segment index here
      layoutAddr = SFE.firstRelAddress 80 (fromIntegral nextSegmentAddress)
      dataAddr = SFE.firstRelAddress 81 (fromIntegral newDataSectionBase)
      Just textSectionAddr = MM.resolveAbsoluteAddr mem (fromIntegral (E.elfSectionAddr textSection))
  (overwrittenBytes, instrumentedBytes, mNewData, newSyms) <- instrumentTextSection cfg mem textSectionAddr (E.elfSectionData textSection) entryPoint strat layoutAddr dataAddr symmap
  (extraTextSecIdx, instrumentationSeg) <- withCurrentELF (newInstrumentationSegment nextSegmentAddress instrumentedBytes)

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
      dataSegAddr <- withCurrentELF (segmentLayoutAddress newDataSectionBase)
      newDataSeg <- withCurrentELF (newDataSegment dataSegAddr newData)
      modifyCurrentELF (appendSegment newDataSeg)
  case mBaseSymtab of
    Nothing -> return ()
    Just baseSymtab -> do
      newSymtab <- withCurrentELF (buildNewSymbolTable (E.elfSectionIndex textSection) extraTextSecIdx layoutAddr newSyms baseSymtab)
      modifyCurrentELF (appendDataRegion (E.ElfDataSymtab newSymtab))
  modifyCurrentELF appendHeaders

  -- Now overwrite the original code (in the .text segment) with the
  -- content computed by our transformation.
  modifyCurrentELF (overwriteTextSection overwrittenBytes)

buildSymbolMap :: Integral (E.ElfWordType w)
               => MM.MemWidth w
               => MM.Memory w
               -> E.Elf w
               -> ElfRewriter w (SFEM.SymbolMap w)
buildSymbolMap mem elf = do
  case filter isSymbolTable (F.toList (E._elfFileData elf)) of
    [E.ElfDataSymtab table] -> do
      let entries = catMaybes (map mkPair (F.toList (E.elfSymbolTableEntries table)))
      return (foldr (uncurry Map.insert) mempty entries)
    -- TODO: can there be more than 1 symbol table?
    _ -> return mempty
  where
  mkPair e = case MM.resolveAbsoluteAddr mem (fromIntegral (E.steValue e)) of
    Just addr | E.steType e == E.STT_FUNC -> Just (SFE.relFromSegmentOff addr, E.steName e)
    _ -> Nothing

isSymbolTable :: E.ElfDataRegion w -> Bool
isSymbolTable (E.ElfDataSymtab{}) = True
isSymbolTable _                   = False

buildNewSymbolTable :: (E.ElfWidthConstraints w, MM.MemWidth w)
                    => Word16
                    -> E.ElfSectionIndex
                    -> SFE.RelAddress w
                    -> SFEM.NewSymbolsMap w
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
                                , e <- maybeToList $! Map.lookup (fromIntegral (SFE.absoluteAddress oa)) t
                                ]

-- | Get the current symbol table
getBaseSymbolTable :: E.Elf w
                   -> ElfRewriter w (Maybe (E.ElfSymbolTable (E.ElfWordType w)))
getBaseSymbolTable = return . listToMaybe . E.elfSymtab

newFromEntry :: MM.MemWidth w
             => E.ElfWidthConstraints w
             => Word16
             -> E.ElfSectionIndex
             -> SFE.RelAddress w
             -> E.ElfSymbolTableEntry (E.ElfWordType w)
             -> SFE.RelAddress w
             -> B.ByteString
             -> E.ElfSymbolTableEntry (E.ElfWordType w)
newFromEntry textSecIdx extraTextSecIdx layoutAddr e addr nm = e
  { E.steName  = "__embrittled_" `B.append` nm
  , E.steValue = fromIntegral absAddr
  , E.steIndex = if absAddr >= SFE.absoluteAddress layoutAddr
                 then extraTextSecIdx
                 else E.ElfSectionIndex textSecIdx
  }
  where
    absAddr = SFE.absoluteAddress addr

-- | Create a new data segment containing a single data section, containing the given bytestring
--
-- It will be placed at the given start address.
newDataSegment :: (Num (E.ElfWordType w), Show (E.ElfWordType w), Integral (E.ElfWordType w), Bits (E.ElfWordType w))
               => Bits (E.ElfWordType w)
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
  traceM $ printf "shstrtabidx = %d" shstrtabidx
  let elfData = [ E.ElfDataSectionHeaders
                , E.ElfDataSectionNameTable shstrtabidx
                , E.ElfDataStrtab strtabidx
                , E.ElfDataSegmentHeaders
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
      traceM ("Overwriting section " ++ show (elfDataRegionName r))
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
segmentLayoutAddress :: Num (E.ElfWordType w)
                     => Integral (E.ElfWordType w)
                     => Word32
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
  traceM ("New text section index: " ++ show txtIdx)
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
instrumentTextSection :: forall i a w arch
                       . (ISA.InstructionConstraints i a,
                          Typeable w,
                          KnownNat w,
                          w ~ MM.RegAddrWidth (MM.ArchReg arch),
                          MM.PrettyF (MM.ArchStmt arch),
                          MM.ArchConstraints arch,
                          MM.RegisterInfo (MM.ArchReg arch),
                          MM.HasRepr (MM.ArchReg arch) MM.TypeRepr,
                          Show (MM.ArchReg arch (MM.BVType (MM.ArchAddrWidth arch))),
                          MM.MemWidth w)
                      => RenovateConfig i a w arch
                      -> MM.Memory w
                      -- ^ The memory space
                      -> MM.MemSegmentOff w
                      -- ^ The address of the start of the text section
                      -> B.ByteString
                      -- ^ The bytes of the text section
                      -> MM.MemSegmentOff w
                      -- ^ The entry point in the text section
                      -> SFE.LayoutStrategy
                      -- ^ The strategy to use for laying out instrumented blocks
                      -> SFE.RelAddress w
                      -- ^ The address to lay out the instrumented blocks
                      -> SFE.RelAddress w
                      -- ^ The address to lay out the new data section
                      -> SFEM.SymbolMap w
                      -- ^ meta data?
                      -> ElfRewriter w (B.ByteString, B.ByteString, Maybe B.ByteString, SFEM.NewSymbolsMap w)
instrumentTextSection cfg mem textSectionAddr textBytes entryPoint strat layoutAddr newGlobalBase symmap = do
  traceM ("instrumentTextSection entry point: " ++ show entryPoint)
  riEntryPointAddress L..= (fromIntegral <$> MM.msegAddr entryPoint)
  let isa = rcISA cfg
      archInfo = rcArchInfo cfg
  case R.recoverBlocks isa (rcDisassembler1 cfg) archInfo mem (entryPoint NEL.:| []) of
    (Left exn1, diags1) -> do
      riBlockRecoveryDiagnostics L..= diags1
      C.throwM (BlockRecoveryFailure exn1 diags1)
    (Right blockInfo, diags1) -> do
      traceM "Recovered blocks with macaw"
      riBlockRecoveryDiagnostics L..= diags1
      let blocks = R.biBlocks blockInfo
      riRecoveredBlocks L..= Just (SomeBlocks (rcISA cfg) blocks)
      let cfgs = FR.recoverFunctions isa mem blockInfo
      case I.runInstrument (SFE.relFromSegmentOff entryPoint) newGlobalBase cfgs (SFE.redirect isa (rcInstrumentor cfg) mem strat layoutAddr blocks symmap) of
        ((Left exn2, _newSyms, diags2), _info) -> do
          riRedirectionDiagnostics L..= diags2
          C.throwM (RewriterFailure exn2 diags2)
        ((Right (overwrittenBlocks, instrumentationBlocks), newSyms, diags2), info) -> do
          riRedirectionDiagnostics L..= diags2
          riInstrumentationInfo L..= Just info
          let allBlocks = overwrittenBlocks ++ instrumentationBlocks
          case cfg of
            RenovateConfig { rcAssembler = asm } -> do
              (overwrittenBytes, instrumentationBytes) <- SFE.assembleBlocks isa textSectionAddr textBytes layoutAddr asm allBlocks
              let newDataBytes = mkNewDataSection newGlobalBase info
              return (overwrittenBytes, instrumentationBytes, newDataBytes, newSyms)

mkNewDataSection :: (MM.MemWidth w) => SFE.RelAddress w -> I.InstrumentInfo w -> Maybe B.ByteString
mkNewDataSection baseAddr info = do
  guard (bytes > 0)
  return (B.pack (replicate bytes 0))
  where
    bytes = fromIntegral (I.nextGlobalAddress info `SFE.addressDiff` baseAddr)

data ElfRewriteException = RewrittenTextSectionSizeMismatch Int Int
                         | StringTableNotFound
                         | BlockRecoveryFailure C.SomeException [R.Diagnostic]
                         | RewriterFailure C.SomeException [SFE.Diagnostic]
                         | UnsupportedArchitecture E.ElfMachine
                         | MemoryLoadError String
                         | NoTextSectionFound
                         | MultipleTextSectionsFound Int
                         deriving (Typeable)

deriving instance Show ElfRewriteException
instance C.Exception ElfRewriteException

newtype ElfRewriter w a = ElfRewriter { unElfRewrite :: S.StateT (RewriterInfo w) P.Catch a }
                          deriving (Monad,
                                    Functor,
                                    Applicative,
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
                                   , _riInstrumentationInfo      = Nothing
                                   , _riELF                      = e
                                   }

riSegmentVirtualAddress :: L.Simple L.Lens (RewriterInfo w) (Maybe Word64)
riSegmentVirtualAddress = L.lens _riSegmentVirtualAddress (\ri v -> ri { _riSegmentVirtualAddress = v })

riOverwrittenRegions :: L.Simple L.Lens (RewriterInfo w) [(String, Word64)]
riOverwrittenRegions = L.lens _riOverwrittenRegions (\ri v -> ri { _riOverwrittenRegions = v })

riAppendedSegments :: L.Simple L.Lens (RewriterInfo w) [(E.ElfSegmentType, Word16, Word64, Word64)]
riAppendedSegments = L.lens _riAppendedSegments (\ri v -> ri { _riAppendedSegments = v })

riEntryPointAddress :: L.Simple L.Lens (RewriterInfo w) (Maybe Word64)
riEntryPointAddress = L.lens _riEntryPointAddress (\ri v -> ri { _riEntryPointAddress = v })

riSectionBaseAddress :: L.Simple L.Lens (RewriterInfo w) (Maybe Word64)
riSectionBaseAddress = L.lens _riSectionBaseAddress (\ri v -> ri { _riSectionBaseAddress = v })

riInitialBytes :: L.Simple L.Lens (RewriterInfo w) (Maybe B.ByteString)
riInitialBytes = L.lens _riInitialBytes (\ri v -> ri { _riInitialBytes = v })

riBlockRecoveryDiagnostics :: L.Simple L.Lens (RewriterInfo w) [R.Diagnostic]
riBlockRecoveryDiagnostics = L.lens _riBlockRecoveryDiagnostics (\ri v -> ri { _riBlockRecoveryDiagnostics = v })

riRedirectionDiagnostics :: L.Simple L.Lens (RewriterInfo w) [SFE.Diagnostic]
riRedirectionDiagnostics = L.lens _riRedirectionDiagnostics (\ri v -> ri { _riRedirectionDiagnostics = v })

riRecoveredBlocks :: L.Simple L.Lens (RewriterInfo w) (Maybe SomeBlocks)
riRecoveredBlocks = L.lens _riRecoveredBlocks (\ri v -> ri { _riRecoveredBlocks = v })

riInstrumentationInfo :: L.Simple L.Lens (RewriterInfo w) (Maybe (I.InstrumentInfo w))
riInstrumentationInfo = L.lens _riInstrumentationInfo (\ri v -> ri { _riInstrumentationInfo = v })
