{-# LANGUAGE FlexibleContexts #-}
-- | This module defines opaque concrete and symbolic address types.
module Renovate.Address (
--  Address(..),
  SymbolicAddress(..),
  RelAddress(..),
  relFromSegmentOff,
  firstRelAddress,
  absoluteAddress,
  addressAddOffset,
  addressDiff
  ) where

import qualified GHC.Err.Located as L

import           Data.Int ( Int64 )
import           Data.Maybe ( fromMaybe )
import           Data.Word ( Word64 )

import qualified Data.Macaw.Memory as MM


-- | Symbolic addresses that can be referenced abstractly and
-- relocated.  They will automatically be concretized when blocks are
-- laid out.
newtype SymbolicAddress = SymbolicAddress Word64
                        deriving (Eq, Ord, Show)

-- | Addresses relative to some base segment index
--
-- These addresses have base addresses.  The base address allows for
-- computing the difference between addresses in different segments.
--
-- These addresses differ from SegmentedAddr in that they do not have
-- a reference to an already-constructed Segment, as we might not yet
-- have one.
--
-- FIXME: Can we add a segment type index so that addresses in
-- different segments can't be confused?
data RelAddress w = RelAddress { relSegment :: MM.SegmentIndex
                               , relBase :: MM.MemWord w
                               , relOffset :: MM.MemWord w
                               }
  deriving (Eq, Ord, Show)

-- | Convert an address in @base + offset@ from macaw ('MM.MemSegmentOff') into
-- our internal representation of addresses
relFromSegmentOff :: (L.HasCallStack, MM.MemWidth w)
                  => MM.MemSegmentOff w
                  -> RelAddress w
relFromSegmentOff so = case MM.viewSegmentOff so of
  (seg,off) -> RelAddress { relSegment = MM.segmentIndex seg
                          , relBase    = fromMaybe err $ MM.segmentBase seg
                          , relOffset  = off
                          }
  where
    err = L.error "relFromSegmentOff: MemSegmentOff swith no base address cannot be converted to RelAddresses"


-- | Construct the first 'RelAddress' from a given base and segment
firstRelAddress :: (MM.MemWidth w) => MM.SegmentIndex -> MM.MemWord w -> RelAddress w
firstRelAddress segIx base = RelAddress { relSegment = segIx
                                        , relBase = base
                                        , relOffset = 0
                                        }

{-# INLINE absoluteAddress #-}
-- | Convert a 'RelAddress' (which is a segment + offset representation) to an
-- absolute address.
absoluteAddress :: (MM.MemWidth w) => RelAddress w -> MM.MemWord w
absoluteAddress a = relBase a + relOffset a

-- | Add an offset to an 'Address'
--
-- It will throw an error if the address underflows and ends up before
-- the base of the segment containing the address.
addressAddOffset :: (L.HasCallStack, MM.MemWidth w) => RelAddress w -> MM.MemWord w -> RelAddress w
addressAddOffset a offset
  | offset < 0 && offset > relOffset a = L.error "Address arithmetic underflow"
  | otherwise = a { relOffset = relOffset a + offset }

-- | Compute the difference between two addresses
addressDiff :: (L.HasCallStack, MM.MemWidth w) => RelAddress w -> RelAddress w -> Int64
addressDiff a1 a2
  | offUnsigned > fromIntegral i64Max = L.error ("addressDiff: difference too large to fit in an Int64 " ++ show (a1, a2))
  | a1 > a2 = fromIntegral offUnsigned
  | otherwise = negate (fromIntegral offUnsigned)
  where
    offUnsigned
      | a1 > a2 = (absoluteAddress a1) - (absoluteAddress a2)
      | otherwise = (absoluteAddress a2) - (absoluteAddress a1)
    i64Max :: Word64
    i64Max = fromIntegral (maxBound :: Int64)
