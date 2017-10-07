{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module defines opaque concrete and symbolic address types.
module Renovate.Address (
--  Address(..),
  SymbolicAddress(..),
  RelAddress,
  relFromSegmentOff,
  firstRelAddress,
  absoluteAddress,
  addressAddOffset,
  addressDiff
  ) where

import qualified GHC.Err.Located as L
import           Control.Exception (assert)

import           Data.Int ( Int64 )
import           Data.Maybe ( fromMaybe )
import           Data.Word ( Word64 )
import qualified Numeric as N

import qualified Data.Macaw.Memory as MM

import qualified Data.Text.Prettyprint.Doc as PD


-- | Symbolic addresses that can be referenced abstractly and
-- relocated.  They will automatically be concretized when blocks are
-- laid out.
newtype SymbolicAddress = SymbolicAddress Word64
                        deriving (Eq, Ord, Show)

instance PD.Pretty SymbolicAddress where
  pretty (SymbolicAddress a) = "0x" PD.<> PD.pretty (N.showHex a "")

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

instance (MM.MemWidth w) => PD.Pretty (RelAddress w) where
  pretty (RelAddress _seg base off) = "0x" PD.<> PD.pretty (N.showHex (base + off) "")

-- | Constructs a canonical RelAddress from a MemSegmentOff.
-- it needs the Memory argument for the canonicalization. We look through
-- the set of available segments to find the closest one and construct
-- the address relative to it.
-- Finally, convert an address in @base + offset@ from macaw ('MM.MemSegmentOff') into
-- our internal representation of addresses
relFromSegmentOff :: (L.HasCallStack, MM.MemWidth w)
             => MM.Memory w
             -> MM.MemSegmentOff w
             -> RelAddress w
relFromSegmentOff mem so = case foldr findClosest firstSeg segBaseIdxs of
  s -> let a  = RelAddress
                { relSegment = MM.segmentIndex s
                , relBase    = fromMaybe err $ MM.segmentBase s
                , relOffset  = off - (fromMaybe err (MM.segmentBase s) - fromMaybe err (MM.segmentBase seg))
                }
           -- A fake RelAddress for the assert
           a' = RelAddress
                { relSegment = 0
                , relBase    = base
                , relOffset  = off
                }
       -- Make sure we haven't munged the absolute address
       in assert (absoluteAddress a == absoluteAddress a') a
  where
  firstSeg = case segBaseIdxs of
             []    -> error "mkRelAddress: No segments"
             (f:_) -> f
  (seg,off)    = MM.viewSegmentOff so
  base         = fromMaybe err $ MM.segmentBase seg
  addr         = base + off
  segBaseIdxs  = MM.memSegments mem
  err          = L.error "mkRelAddress: MemSegmentOffs with no base address cannot be converted to RelAddresses"
  -- The logic here is:
  -- if the base we're considering is less than the absolute address of
  -- the MemSegmentOff (so), but greater than our current closest base
  -- then update to use the segment. Otherwise, keep the one we have.
  findClosest s acc | Just bs   <- MM.segmentBase s
                    , Just bacc <- MM.segmentBase acc
                    , bs <= addr && bs `isCloserThan` bacc = s
                    | otherwise                            = acc
    where
    x `isCloserThan` y = abs (addr - x) < abs (addr - y)

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
addressAddOffset :: (L.HasCallStack, MM.MemWidth w) => MM.Memory w -> RelAddress w -> MM.MemWord w -> RelAddress w
addressAddOffset mem a offset =
  case MM.lookupSegment mem (relSegment a) of
    -- We don't have a segment for this address, so this is likely
    -- an address into the newly allocated blocks. Just update the offset.
    Nothing       -> a { relOffset = relOffset a + offset }
    -- Try to keep the address canonicalized
    Just firstSeg -> case foldr findClosest firstSeg segBaseIdxs of
      s -> let base = fromMaybe (relBase a) (MM.segmentBase s)
               off  = addr - base
               a'   = RelAddress
                      { relSegment = MM.segmentIndex s
                      , relBase    = base
                      , relOffset  = off
                      }
           in assert (absoluteAddress a + offset == absoluteAddress a') a'
  where
  addr         = absoluteAddress a + offset
  segBaseIdxs  = MM.memSegments mem
  findClosest s acc | Just bs   <- MM.segmentBase s
                    , Just bacc <- MM.segmentBase acc
                    , bs <= addr && bs `isCloserThan` bacc = s
                    | otherwise                            = acc
    where
    x `isCloserThan` y = abs (addr - x) < abs (addr - y)

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
