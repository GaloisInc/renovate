{-# LANGUAGE FlexibleContexts #-}

module Renovate.BinaryFormat.ELF.Internal
  ( findSpaceForPHDRs
  , LoadSegmentInfo
  , makeLoadSegmentInfo
  ) where

import qualified Data.List.NonEmpty as NEL

import qualified Data.ElfEdit as E

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

type PHDRAddressSelectionError = () -- TODO(lb)

-- | Find a spot in the program's virtual address space for the new PHDR segment
--
-- This is an implementation of the core of 'choosePHDRSegmentAddress', see the
-- comment there for more detail.
--
-- * The address of the PHDR table must be >= its offset in the file (the kernel
--   doesn't check and does an invalid subtraction otherwise)
findSpaceForPHDRs ::
  (Num (E.ElfWordType w)) =>
  NEL.NonEmpty (LoadSegmentInfo w) {-^ Info about other LOAD segments -} ->
  E.ElfWordType w {- ^ Offset of PHDR in file -} ->
  E.ElfWordType w {- ^ Size of PHDR segment -} ->
  Either PHDRAddressSelectionError (E.ElfWordType w)
findSpaceForPHDRs = undefined


  -- TODO(lb): What about addresses before the first segment? After the last?
  -- TODO(lb): How to make sure PHDR doesn't collide with the heap?
  -- let minGap =
  --       minimum $ map (\phdr -> E.phdrSegmentVirtAddr phdr - (E.fromFileOffset (E.phdrFileStart phdr))) phdrs

  -- Look for the range that's closest to the optimal virtual address, i.e. the
  -- file offset of the fake PHDR segment.
  --
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
