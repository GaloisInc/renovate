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
  ( alignValue
  , allocatedVAddrs
  , allocatedVAddrsM
  , findTextSections
  , findTextSection
  , TextSectionIssue
  , newTextAlign
  ) where

import qualified Control.Monad.Catch as C
import qualified Control.Monad.Fail as Fail
import qualified Data.ByteString.Char8 as C8
import qualified Data.Foldable as F
import qualified Data.Map as Map
import           Data.Typeable (Typeable)
import           Data.Word (Word64)

import qualified Data.ElfEdit as E

-- | Align a value
--
-- @alignValue v alignment@ returns the value @v'@ greater than or equal to @v@
-- such that @v' % align == 0@.
--
-- For an alignment of zero or one, return @v@.
alignValue :: (Integral w) => w -> w -> w
alignValue v 0 = v
alignValue v alignment = v + ((alignment - (v `mod` alignment)) `mod` alignment)

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
allocatedVAddrsM ::
  Fail.MonadFail m =>
  E.ElfWidthConstraints w =>
  E.Elf w ->
  m (Map.Map (E.ElfWordType w) (E.ElfWordType w))
allocatedVAddrsM e = case allocatedVAddrs e of
  Left seg -> fail
    $  "Could not compute free virtual addresses: segment "
    ++ show (E.elfSegmentIndex seg)
    ++ " has relative size"
  Right m -> return m

findTextSections :: E.Elf w -> [E.ElfSection (E.ElfWordType w)]
findTextSections = E.findSectionByName (C8.pack ".text")

data TextSectionIssue =
    NoTextSectionFound
  | MultipleTextSectionsFound Int
  deriving (Show, Typeable)

instance C.Exception TextSectionIssue

findTextSection :: C.MonadThrow m => E.Elf w -> m (E.ElfSection (E.ElfWordType w))
findTextSection e = do
  case findTextSections e of
    [textSection] -> return textSection
    [] -> C.throwM NoTextSectionFound
    sections -> C.throwM (MultipleTextSectionsFound (length sections))

-- | The alignment of the new text segment
--
-- We could just copy the alignment from the old one, but the alignment on x86
-- is very high, which would waste a lot of space.  It seems like setting it
-- lower is safe...
newTextAlign :: Word64
newTextAlign = 0x1000
