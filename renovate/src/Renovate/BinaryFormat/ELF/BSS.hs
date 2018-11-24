{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
-- | This module contains the code that fixes up the .bss for our rewritten binaries
module Renovate.BinaryFormat.ELF.BSS (
  expandBSS
  ) where

import qualified Control.Lens as L
import qualified Control.Monad.State.Strict as S
import qualified Data.ByteString as B
import qualified Data.ElfEdit as E
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Macaw.CFG as MM
import           Data.Maybe ( catMaybes )
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import           Data.Word ( Word16 )

import           Renovate.BinaryFormat.ELF.Rewriter

-- | The .bss is a special data section that contains zero-initialized data.
-- Since the data is known, it doesn't need to be explicitly present in the
-- binary, saving space.  Normally, the kernel maps and initializes the .bss for
-- us.  Unfortunately, there are some undocumented limitations on the kernel's
-- .bss handling: it maintains a high watermark for mapped segment addresses and
-- only maps and initializes bss if it is greater than the highest seen segment
-- address.  Because of our handling of the program headers (we place them at a
-- high address, but at the beginning of the file), this means that the bss is
-- never initialized.
--
-- To compensate for this, we change the bss from an implicit (NOBITS)
-- representation to an explicit representation by expanding the data section to
-- cover the bss region with explicitly zero-initialized data.  One slight
-- tricky bit is that we need to add some extra padding after the new data to
-- maintain the alignment of all of the sections that follow.
expandBSS :: ( w ~ MM.ArchAddrWidth arch
             , E.ElfWidthConstraints w
             )
          => E.Elf w
          -> ElfRewriter arch ((), E.Elf w)
expandBSS e0 = do
  -- First, discard all of the existing BSS sections in each segment and fix the
  -- segment size to be 'E.ElfRelativeSize'.  It isn't clear if that is
  -- required, but it is morally right.
  let (e1, nobitsSections) = S.runState (E.traverseElfSegments discardBSS e0) []
  -- Now fix up the section IDs, since we removed some sections.  We need them
  -- to be contiguous for later bookkeeping
  e2 <- fixPostBSSSectionIds nobitsSections e1
  -- Expand the data section to cover the .bss region, while also adding enough
  -- extra data to preserve the alignment of the rest of the sections later in
  -- the binary.
  ((),) <$> expandPreBSSDataSection nobitsSections e2
  where
    discardBSS seg
      | Just nobitsSections <- trailingNobitsSections seg = do
          S.put nobitsSections
          segContents' <- catMaybes <$> mapM (updateRegionSections (dropTrailingNobits nobitsSections)) (F.toList (E.elfSegmentData seg))
          return seg { E.elfSegmentData = Seq.fromList segContents'
                     , E.elfSegmentMemSize = E.ElfRelativeSize 0
                     }
      | otherwise = return seg

sequential :: (Num a, Eq a) => [a] -> Bool
sequential [] = True
sequential [_] = True
sequential (a : rest@(b : _)) = a + 1 == b && sequential rest

-- | Expands the data section immediately before the BSS to cover the address
-- range of all of the BSS sections.  The data is explicitly zero-initialized.
--
-- NOTE: This currently works based on section numbers rather than addresses
expandPreBSSDataSection :: ( w ~ MM.ArchAddrWidth arch
                           , E.ElfWidthConstraints w
                           )
                        => [E.ElfSection (E.ElfWordType w)]
                        -> E.Elf w
                        -> ElfRewriter arch (E.Elf w)
expandPreBSSDataSection (L.sortOn E.elfSectionIndex -> nobitsSections) e0 = do
  assertM (sequential (map E.elfSectionIndex nobitsSections))
  case nobitsSections of
    [] -> return e0
    firstBss : _ -> do
      let prevSecId = E.elfSectionIndex firstBss - 1
      L.traverseOf E.elfSections (expandIfPrev maxPostBSSAlign prevSecId nobitsSections) e0
  where
    (_, maxPostBSSAlign) = foldr maxAlignIfAfterBSS (False, 0) (L.toListOf E.elfSections e0)
    maxAlignIfAfterBSS sec (seenBSS, align)
      | seenBSS && E.elfSectionIndex sec `notElem` map E.elfSectionIndex nobitsSections =
        (True, max align (E.elfSectionAddrAlign sec))
      | otherwise = (seenBSS || E.elfSectionIndex sec `elem` map E.elfSectionIndex nobitsSections, align)

expandIfPrev :: (Monad m, Integral w)
             => w
             -> Word16
             -> [E.ElfSection w]
             -> E.ElfSection w
             -> m (E.ElfSection w)
expandIfPrev maxPostBSSAlign prevSecId nobitsSections sec
  | E.elfSectionIndex sec /= prevSecId = return sec
  | otherwise = do
      let dataEnd = sectionEnd sec
      let bssEnd = maximum (map sectionEnd nobitsSections)
      -- extraBytes is the number of bytes from the end of the data section to cover the entire bss
      let extraBytes = bssEnd - dataEnd
      -- paddingBytes come after the bss to properly align the rest of the
      -- sections that come after data (which formerly had been aligned to the
      -- end of data).  We do the max with 1 to ensure we don't divide by zero.
      let realAlign = max 1 maxPostBSSAlign
      let paddingBytes = realAlign - (extraBytes `mod` realAlign)
      let newData = B.replicate (fromIntegral (extraBytes + paddingBytes)) 0 -- bssEnd - dataEnd)) 0
      let secData = E.elfSectionData sec <> newData
      return sec { E.elfSectionData = secData
                 , E.elfSectionSize = fromIntegral (B.length secData)
                 }
  where
    sectionEnd s = E.elfSectionAddr s + E.elfSectionSize s

fixPostBSSSectionIds :: [E.ElfSection (E.ElfWordType w)]
                     -> E.Elf w
                     -> ElfRewriter arch (E.Elf w)
fixPostBSSSectionIds nobitsSections e0
  | null nobitsSections = return e0
  | otherwise = do
      e1 <- L.traverseOf E.elfSections secFixIfPostBSS e0
      L.traverseOf E.traverseElfDataRegions regionFixIfPostBSS e1
  where
    lastBSSSectionId = maximum (map E.elfSectionIndex nobitsSections)
    numBSS = fromIntegral (length nobitsSections)
    secFixIfPostBSS sec
      | E.elfSectionIndex sec > lastBSSSectionId =
        return sec { E.elfSectionIndex = E.elfSectionIndex sec - numBSS }
      | otherwise = return sec
    regionFixIfPostBSS r =
      case r of
        E.ElfDataSectionNameTable ix
          | ix > lastBSSSectionId -> return (E.ElfDataSectionNameTable (ix - numBSS))
          | otherwise -> return r
        E.ElfDataStrtab ix
          | ix > lastBSSSectionId -> return (E.ElfDataStrtab (ix - numBSS))
          | otherwise -> return r
        E.ElfDataGOT got
          | E.elfGotIndex got > lastBSSSectionId -> do
            let got' = got { E.elfGotIndex = E.elfGotIndex got - numBSS }
            return (E.ElfDataGOT got')
          | otherwise -> return r
        E.ElfDataSymtab st
          | E.elfSymbolTableIndex st > lastBSSSectionId -> do
            let st' = st { E.elfSymbolTableIndex = E.elfSymbolTableIndex st - numBSS }
            return (E.ElfDataSymtab st')
          | otherwise -> return r
        E.ElfDataSegmentHeaders {} -> return r
        E.ElfDataSegment {} -> return r
        E.ElfDataSectionHeaders {} -> return r
        E.ElfDataElfHeader {} -> return r
        -- Sections are taken care of in the step before this
        E.ElfDataSection {} -> return r
        E.ElfDataRaw {} -> return r

-- | Return the list of section indexes that occur at the end of the segment.
--
-- Specifically, we are looking for all of the NOBITS sections at the end of a
-- segment (where they are the last sections assigned addresses).  Additionally,
-- we require at least one to be named .bss
--
-- NOTE: This is kind of a proxy test, but is probably good enough in practice.
-- What we really want to test is if this is the last segment /and/ it ends in
-- at least one section that is 1) of type NOBITS, and 2) has a virtual address.
--
-- Unfortunately, the full test is kind of difficult given the existing
-- combinators.  In any realistic binary, we can identify this condition as just
-- having the .bss section, as libc will trigger that condition.
trailingNobitsSections :: (E.ElfWidthConstraints w) => E.ElfSegment w -> Maybe [E.ElfSection (E.ElfWordType w)]
trailingNobitsSections seg =
  let (foundBSS, ixs) = go (False, []) (E.ElfDataSegment seg)
  in if | null ixs && foundBSS -> error "Found a BSS, but the list of nobits sections is empty"
        | foundBSS -> Just ixs
        | otherwise -> Nothing
  where
    isBSS sec = E.elfSectionName sec == ".bss"
    go acc@(foundBSS, ixs) r =
      case r of
        E.ElfDataSegment seg' -> F.foldl' go acc (E.elfSegmentData seg')
        E.ElfDataSection sec
          | and [ E.elfSectionType sec == E.SHT_NOBITS
                , E.elfSectionAddr sec /= 0
                ] -> (foundBSS || isBSS sec, sec : ixs)
          | E.elfSectionAddr sec == 0 -> (foundBSS || isBSS sec, ixs)
          | otherwise -> (foundBSS || isBSS sec, [])
        _ -> acc

dropTrailingNobits :: (Monad f) => [E.ElfSection w] -> E.ElfSection w -> f (Maybe (E.ElfSection w))
dropTrailingNobits nobitsSections sec
  | E.elfSectionIndex sec `elem` fmap E.elfSectionIndex nobitsSections = return Nothing
  | otherwise = return (Just sec)

-- | A 'L.Traversal' over sections within a data region
--
-- This traversal allows for the removal of sections.
updateRegionSections :: L.Traversal (E.ElfDataRegion w)
                                    (Maybe (E.ElfDataRegion w))
                                    (E.ElfSection (E.ElfWordType w))
                                    (Maybe (E.ElfSection (E.ElfWordType w)))
updateRegionSections f edr =
  case edr of
    E.ElfDataSegment seg ->
      let toRegion c = E.ElfDataSegment (seg { E.elfSegmentData = Seq.fromList c })
      in Just <$> toRegion <$> catMaybes <$> T.traverse (updateRegionSections f) (F.toList (E.elfSegmentData seg))
    E.ElfDataSection sec -> fmap E.ElfDataSection <$> f sec
    _ -> pure (Just edr)
