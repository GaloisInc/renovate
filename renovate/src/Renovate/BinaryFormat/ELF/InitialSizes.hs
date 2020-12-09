-- | Computation of initial sizes of sections and segments in an ELF file
--
-- The rewriter needs add sections and segments to the binary, which changes the
-- sizes of some *dynamically-sized* regions.  Doing so carelessly would shift
-- the rest of the contents of the binary, breaking alignment requirements.  To
-- avoid this, we need to replace those dynamically-sized sections/segments with
-- padding.
--
-- This module computes the sizes of every segment/section of the initial
-- binary, which will inform the replacement process during rewriting.
module Renovate.BinaryFormat.ELF.InitialSizes (
  InitialSizes(..),
  Extent(..),
  computeInitialSizes
  ) where

import qualified Data.ElfEdit as E
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Vector as V
import           Data.Word ( Word32, Word16 )

data Extent w = Extent { offset :: !(E.ElfWordType w)
                       , size :: !(E.ElfWordType w)
                       }

data InitialSizes w =
  InitialSizes { sectionExtents :: !(Map.Map Word16 (Extent w))
               -- ^ A map from section indexes to their extents
               , segmentExtents :: !(Map.Map Word16 (Extent w))
               , sectionHeaderTable :: !(Extent w)
               , programHeaderTable :: !(Extent w)
               }

computeInitialSizes :: E.ElfHeaderInfo w -> InitialSizes w
computeInitialSizes ehi =
  InitialSizes { sectionExtents = V.ifoldl' addSectionExtent Map.empty (E.headerShdrs ehi)
               , segmentExtents = F.foldl' addSegmentExtent Map.empty (E.headerPhdrs ehi)
               , sectionHeaderTable = fromFileRange (E.shdrTableRange ehi)
               , programHeaderTable = fromFileRange (E.phdrTableRange ehi)
               }
  where
    fromFileRange (E.FileOffset off, sz) =
      Extent { offset = off, size = sz }

addSegmentExtent :: Map.Map Word16 (Extent w)
                 -> E.Phdr w
                 -> Map.Map Word16 (Extent w)
addSegmentExtent m phdr =
  Map.insert (E.phdrSegmentIndex phdr) ext m
  where
    ext = Extent { offset = E.fromFileOffset (E.phdrFileStart phdr)
                 , size = E.phdrFileSize phdr
                 }

addSectionExtent :: Map.Map Word16 (Extent w)
                 -> Int
                 -> E.Shdr Word32 (E.ElfWordType w)
                 -> Map.Map Word16 (Extent w)
addSectionExtent m idx shdr =
  Map.insert (fromIntegral idx) ext m
  where
    ext = Extent { offset = E.fromFileOffset (E.shdrOff shdr)
                 , size = E.shdrSize shdr
                 }
