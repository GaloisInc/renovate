{-# LANGUAGE FlexibleContexts #-}

module Renovate.BinaryFormat.ELF.Internal
  ( findSpaceForPHDRs
  , LoadSegmentInfo(..)
  , makeLoadSegmentInfo
  ) where

import qualified Data.Ord as O
import qualified Data.List.NonEmpty as NEL

import qualified Data.ElfEdit as E

import           Renovate.BinaryFormat.ELF.Common
import           Renovate.BinaryFormat.ELF.Common.Internal

-- | Information about other LOAD segments that's needed to select a good spot
-- for the PHDR segment.
data LoadSegmentInfo w =
  LoadSegmentInfo
    { pOffset :: E.ElfWordType w
    , pVAddr :: E.ElfWordType w
    , pMemSz :: E.ElfWordType w
    }

makeLoadSegmentInfo :: E.Phdr w -> Maybe (LoadSegmentInfo w)
makeLoadSegmentInfo phdr =
  case E.phdrSegmentType phdr of
    E.PT_PHDR -> Just $ LoadSegmentInfo { pOffset =
                                            let E.FileOffset off = E.phdrFileStart phdr
                                            in off
                                        , pVAddr = E.phdrSegmentVirtAddr phdr
                                        , pMemSz = E.phdrMemSize phdr
                                        }
    _ -> Nothing

-- | Find a spot in the program's virtual address space for the new PHDR segment
--
-- This is an implementation of the core of 'choosePHDRSegmentAddress', see the
-- comment there for more detail.
--
-- The address of the PHDR table must be >= its offset in the file (the kernel
-- doesn't check and does an invalid subtraction otherwise).
--
-- This function assumes:
--
-- 1. There are no segments whose images overlap in the virtual address space
findSpaceForPHDRs ::
  (Num (E.ElfWordType w), Ord (E.ElfWordType w)) =>
  NEL.NonEmpty (LoadSegmentInfo w) {-^ Info about other LOAD segments -} ->
  E.ElfWordType w {- ^ Offset of PHDR in file -} ->
  E.ElfWordType w {- ^ Size of PHDR segment -} ->
  Maybe (E.ElfWordType w)
findSpaceForPHDRs segInfos phdrOffset phdrSize =
  let sortedSegInfos@(firstSegment NEL.:| _) =
        NEL.sortBy (O.comparing pVAddr) segInfos
      last = NEL.head (NEL.reverse sortedSegInfos)
      -- The optimal address is either:
      --
      -- 1. before the first segment,
      -- 2. after the last segment, or
      -- 3. between two segments
      beforeFirst =
        if pVAddr firstSegment > phdrSize + (fromIntegral pageAlignment)
        then Just $ alignDown (pVAddr firstSegment - phdrSize) pageAlignment
        else Nothing
      afterLast = align (pVAddr )
      between = _
      candidateAddresses = maybeToList beforeFirst ++ (afterLast : concat between)
      bestCandidate = _
      minGap =
          minimum $ fmap (\segInfo -> pVAddr segInfo - pOffset segInfo) segInfos
  in if minGap < bestCandidate - phdrOffset
     then Nothing
     else Just bestCandidate

  -- NB: We could do this in linear time since this list is ordered by start
  -- address, but that seems like a pain and there probably aren't many segments
  -- at all.
  -- let E.FileOffset projectedOffset = E.phdrFileStart fakePhdrSegment
  -- let deltaFromOptimal addr = abs (addr - fromIntegral projectedOffset)
  -- let closest =
  --       L.minimumBy (O.comparing (\(lo, hi) -> min (deltaFromOptimal lo) (deltaFromOptimal hi))) ranges


  -- Now, find any addresses that are between existing segments, have enough
  -- space for the new segment, and are aligned properly.
  -- let ranges = [ (loAddr, hiAddr)
  --              | (loSeg, hiSeg) <- zip phdrs (drop 1 phdrs)
  --              , let hiAddr = E.phdrSegmentVirtAddr hiSeg
  --              , let loAddr =
  --                      alignValue
  --                        (E.phdrSegmentVirtAddr loSeg + E.phdrMemSize loSeg)
  --                        (fromIntegral pageAlignment)
  --              , requiredSize < (hiAddr - loAddr)
  --              ]
