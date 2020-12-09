{-
Module           : Renovate.BinaryFormat.ELF.Internal
Description      : Internal modules, exposed for testing only. Do not import!
Copyright        : (c) Galois, Inc 2020
License          : BSD3
Maintainer       : Langston Barrett <langston@galois.com>
Stability        : provisional
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Renovate.BinaryFormat.ELF.Internal
  ( findSpaceForPHDRs
  , LoadSegmentInfo(..)
  , makeLoadSegmentInfo
  , PHDRAddress(..)
  ) where

import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
import qualified Data.Ord as O

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

deriving instance Eq (E.ElfWordType w) => Eq (LoadSegmentInfo w)
deriving instance Ord (E.ElfWordType w) => Ord (LoadSegmentInfo w)
deriving instance Show (E.ElfWordType w) => Show (LoadSegmentInfo w)

makeLoadSegmentInfo :: E.Phdr w -> Maybe (LoadSegmentInfo w)
makeLoadSegmentInfo phdr =
  if E.phdrSegmentType phdr /= E.PT_LOAD
  then Nothing
  else Just $ LoadSegmentInfo { pOffset =
                                  let E.FileOffset off = E.phdrFileStart phdr
                                  in off
                              , pVAddr = E.phdrSegmentVirtAddr phdr
                              , pMemSz = E.phdrMemSize phdr
                              }

data PHDRAddress w = TLSSafeAddress (E.ElfWordType w)
                   | FallbackAddress (E.ElfWordType w)
                   | NoAddress

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
-- given offset (see commentary on 'doRewrite' in "Renovate.BinaryFormat.ELF").
findSpaceForPHDRs ::
  (E.ElfWidthConstraints w) =>
  NEL.NonEmpty (LoadSegmentInfo w) {-^ Info about other LOAD segments -} ->
  E.ElfWordType w {- ^ Offset of PHDR in file -} ->
  E.ElfWordType w {- ^ Size of PHDR segment -} ->
  PHDRAddress w
findSpaceForPHDRs segInfos phdrOffset phdrSize =
  let sortedSegInfos@(firstSegment NEL.:| _) =
        NEL.sortBy (O.comparing pVAddr) segInfos
      lastSegment = NEL.head (NEL.reverse sortedSegInfos)
      pgAlign = fromIntegral pageAlignment
      -- The optimal address is either:
      --
      -- 1. before the first segment,
      -- 2. after the last segment, or
      -- 3. between two segments,
      beforeFirst =
        if pVAddr firstSegment > phdrSize + pgAlign
        then Just $ alignValueDown (pVAddr firstSegment - phdrSize - 1) pgAlign
        else Nothing
      afterLast = Maybe.catMaybes
        [ -- Immediately after
          Just $ alignValue (pVAddr lastSegment + pMemSz lastSegment + 1) pgAlign
        , -- Possibly much further after, if the provided PHDR segment offset is
          -- higher than the range covered by the last segment.
          if phdrOffset > pVAddr lastSegment + pMemSz lastSegment
          then Just $ alignValue phdrOffset pgAlign
          else Nothing
        ]
      -- We don't have to check every address between two segments, just the
      -- maximal and minimal ones.
      --
      -- In detail: For every pair (loSeg, hiSeg) of segments that are adjacent
      -- in the virtual address space,
      -- * Find the lowest aligned address after the end of loSeg. If this
      --   address plus the size of the PHDRs is less than the address of hiSeg,
      --   add it to the set of candidate addresses.
      -- * Find the highest aligned address before the start of hiSeg minus the
      --   size of the PHDRs. If this address is higher than the end of loSeg,
      --   add it to the set of candidate addresses.
      --
      -- In a later step, we winnow down these candidates to find the optimal
      -- address.
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

      rawCandidates = Maybe.maybeToList beforeFirst ++ between ++ afterLast
      validCandidates = filter (>= phdrOffset) rawCandidates
  in
     if null validCandidates
     then NoAddress
     else
       let best = L.minimumBy (O.comparing (\addr -> abs (addr - phdrOffset))) validCandidates
       in if let m = minimum (fmap (\segInfo -> abs (pVAddr segInfo - pOffset segInfo)) segInfos)
             in m < abs (best - phdrOffset)
          then FallbackAddress best
          else TLSSafeAddress best
