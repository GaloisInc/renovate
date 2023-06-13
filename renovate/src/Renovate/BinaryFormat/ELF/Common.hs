{-
Module           : Renovate.BinaryFormat.ELF.Common
Description      : Common operations for dealing with ELF files
Copyright        : (c) Galois, Inc 2020
License          : BSD3
Maintainer       : Langston Barrett <langston@galois.com>
Stability        : provisional
-}

{-# LANGUAGE FlexibleContexts #-}

module Renovate.BinaryFormat.ELF.Common
  ( module Renovate.BinaryFormat.ELF.Common.Internal
  , allocatedVAddrs
  , allocatedVAddrsM
  , findTextSections
  , findTextSection
  , pageAlignment
  , newTextAlign
  ) where

import qualified Control.Monad.Catch as C
import qualified Data.ByteString.Char8 as C8
import qualified Data.Foldable as F
import qualified Data.Map as Map
import           Data.Word (Word64)

import qualified Data.ElfEdit as E

import           Renovate.BinaryFormat.ELF.Common.Internal
import qualified Renovate.Core.Exception as RCE

-- | Extract all the segments' virtual addresses (keys) and their sizes
-- (values). If we don't know the size of a segment yet because it is going to
-- be computed later, return that segment as an error.
allocatedVAddrs ::
  E.ElfWidthConstraints w =>
  E.Elf w ->
  Either (E.ElfSegment w)
         (Map.Map (E.ElfWordType w) (E.ElfWordType w))
allocatedVAddrs e = F.foldl' (Map.unionWith max) Map.empty <$> traverse processRegion (E._elfFileData e) where
  processRegion (E.ElfDataSegment seg) = case E.elfSegmentMemSize seg of
    E.ElfRelativeSize{} -> Left seg
    E.ElfAbsoluteSize size -> return (Map.singleton (E.elfSegmentVirtAddr seg) size)
  processRegion _ = return Map.empty


-- | Like allocatedVAddrs, but throw an error instead of returning it purely
allocatedVAddrsM
  :: (C.MonadThrow m, E.ElfWidthConstraints w)
  => E.Elf w
  -> m (Map.Map (E.ElfWordType w) (E.ElfWordType w))
allocatedVAddrsM e = case allocatedVAddrs e of
  Left seg -> C.throwM (RCE.SegmentHasRelativeSize (toInteger (E.elfSegmentIndex seg)))
  Right m -> return m

findTextSections :: String -> E.Elf w -> [E.ElfSection (E.ElfWordType w)]
findTextSections secName = E.findSectionByName (C8.pack secName)

findTextSection :: C.MonadThrow m => String -> E.Elf w -> m (E.ElfSection (E.ElfWordType w))
findTextSection secName e = do
  case findTextSections secName e of
    [textSection] -> return textSection
    [] -> C.throwM (RCE.MissingExpectedSection secName)
    sections -> C.throwM (RCE.MultipleSectionDefinitions secName (length sections))

-- | The system page alignment (assuming 4k pages)
pageAlignment :: Word64
pageAlignment = 0x1000

-- | The alignment of the new text segment
--
-- We could just copy the alignment from the old one, but the alignment on x86
-- is very high, which would waste a lot of space.  It seems like setting it
-- lower is safe...
newTextAlign :: Word64
newTextAlign = pageAlignment
