{-# LANGUAGE FlexibleContexts #-}

module Renovate.BinaryFormat.ELF.Internal
  ( findSpaceForPHDRs
  , LoadSegmentInfo(..)
  , makeLoadSegmentInfo
  ) where

import qualified Data.Ord as O
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe

import qualified Data.ElfEdit as E

import           Renovate.BinaryFormat.ELF.Common

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
-- This function assumes:
--
-- * There are no segments whose images overlap in the virtual address space
--
-- It guarantees that the address it returns is greater than or equal to the
-- given offset (see commentary in "Renovate.BinaryFormat.ELF").
findSpaceForPHDRs ::
  (Num (E.ElfWordType w), Ord (E.ElfWordType w), Integral (E.ElfWordType w)) =>
  NEL.NonEmpty (LoadSegmentInfo w) {-^ Info about other LOAD segments -} ->
  E.ElfWordType w {- ^ Offset of PHDR in file -} ->
  E.ElfWordType w {- ^ Size of PHDR segment -} ->
  Maybe (E.ElfWordType w)
findSpaceForPHDRs segInfos phdrOffset phdrSize =
  let sortedSegInfos@(firstSegment NEL.:| _) =
        NEL.sortBy (O.comparing pVAddr) segInfos
      lastSegment = NEL.head (NEL.reverse sortedSegInfos)
      pgAlign = fromIntegral pageAlignment
      -- The optimal address is either:
      --
      -- 1. before the first segment,
      -- 2. after the last segment, or
      -- 3. between two segments
      beforeFirst =
        if pVAddr firstSegment > phdrSize + pgAlign
        then Just $ alignValueDown (pVAddr firstSegment - phdrSize - 1) pgAlign
        else Nothing
      afterLast = alignValue (pVAddr lastSegment + pMemSz lastSegment + 1) pgAlign
      -- We don't have to check every address between two segments, just the
      -- maximal and minimal ones.
      between =
        let zipped = zip (NEL.toList sortedSegInfos)
                         (drop 1 (NEL.toList sortedSegInfos))
        in concat [ [ addr
                    | (loSeg, hiSeg) <- zipped
                    , let addr =
                            alignValue (pVAddr loSeg + pMemSz loSeg + 1) pgAlign
                    , phdrSize < pVAddr hiSeg - addr
                    ]
                  , [ addr
                    | (loSeg, hiSeg) <- zipped
                    , let addr =
                            alignValueDown (pVAddr hiSeg - phdrSize - 1) pgAlign
                    , addr > pVAddr loSeg + pMemSz loSeg
                    ]
                  ]

      rawCandidates = afterLast : (Maybe.maybeToList beforeFirst ++ between)
      validCandidates = filter (> phdrOffset) rawCandidates
  in
     if length validCandidates == 0
     then Nothing
     else
       let best = L.minimumBy (O.comparing (\addr -> abs (addr - phdrOffset))) validCandidates
       in if minimum (fmap (\segInfo -> abs (pVAddr segInfo - pOffset segInfo)) segInfos) <
               best - phdrOffset
          then Nothing
          else Just best
