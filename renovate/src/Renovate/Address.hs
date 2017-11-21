{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module defines opaque concrete and symbolic address types.
module Renovate.Address (
  SymbolicAddress(..),
  ConcreteAddress,
  concreteFromSegmentOff,
  concreteFromAbsolute,
  concreteAsSegmentOff,
  absoluteAddress,
  addressAddOffset,
  addressDiff
  ) where

import qualified GHC.Err.Located as L

import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Word ( Word64 )
import qualified Numeric as N

import qualified Data.Macaw.Memory as MM



-- | Symbolic addresses that can be referenced abstractly and
-- relocated.  They will automatically be concretized when blocks are
-- laid out.
newtype SymbolicAddress = SymbolicAddress Word64
                        deriving (Eq, Ord, Show)

instance PD.Pretty SymbolicAddress where
  pretty (SymbolicAddress a) = "0x" PD.<> PD.pretty (N.showHex a "")

-- | The type of concrete addresses that can be laid out in memory
--
-- These addresses have a fixed (and shared) base address, from which they are
-- offset.
--
-- The current backing representation is 'MM.MemAddr', but with the additional
-- guarantee that the 'MM.addrBase' is zero (i.e., an "absolute" address).
-- These addresses are not truly absolute, as they will be relocated when mapped
-- into memory.  They will, however, remain the same distance from one another.
-- We can use this representation (as opposed to 'MM.MemSegmentOff') because we
-- will only ever rewrite single modules (i.e., a single binary or shared
-- library), and do not need inter-module references.  This representation
-- discards segment information, which we can recover with a 'MM.Memory' object
-- and the 'MM.resolveAbsoluteAddr' function.
newtype ConcreteAddress w = ConcreteAddress (MM.MemAddr w)
  deriving (Eq, Ord, Show)

instance (MM.MemWidth w) => PD.Pretty (ConcreteAddress w) where
  pretty (ConcreteAddress memAddr) = PD.pretty (show memAddr)

-- | Construct a 'ConcreteAddress' from a 'MM.MemSegmentOff'
--
-- This can fail if the 'MM.MemSegmentOff' is not an absolute address (i.e., it
-- is a pointer to a location in another module)
concreteFromSegmentOff :: (L.HasCallStack, MM.MemWidth w)
                       => MM.Memory w
                       -> MM.MemSegmentOff w
                       -> Maybe (ConcreteAddress w)
concreteFromSegmentOff _mem segOff = do
  let memAddr = MM.relativeSegmentAddr segOff
  _ <- MM.asAbsoluteAddr memAddr
  return (ConcreteAddress memAddr)

concreteAsSegmentOff :: (MM.MemWidth w) => MM.Memory w -> ConcreteAddress w -> Maybe (MM.MemSegmentOff w)
concreteAsSegmentOff mem (ConcreteAddress memAddr) = MM.asSegmentOff mem memAddr

-- | Construct a 'ConcreteAddress' from a concrete address specified as a
-- 'MM.MemWord'
--
-- This always succeeds, as we just treat a number as an absolute address.
--
-- This function does not ensure that the address is mapped in the underlying
-- 'MM.Memory'.
concreteFromAbsolute :: (MM.MemWidth w)
                     => MM.MemWord w
                     -> ConcreteAddress w
concreteFromAbsolute = ConcreteAddress . MM.absoluteAddr

-- | Convert a 'ConcreteAddress' into an absolute address (a 'MM.MemWord')
--
-- This always succeeds, because our 'ConcreteAddress' is a wrapper that
-- guarantees we only have absolute addresses.
absoluteAddress :: (MM.MemWidth w) => ConcreteAddress w -> MM.MemWord w
absoluteAddress (ConcreteAddress memAddr) = absAddr
  where
    Just absAddr = MM.asAbsoluteAddr memAddr
{-# INLINE absoluteAddress #-}

-- | Add an offset to a 'ConcreteAddress'
--
-- Since all of our 'ConcreteAddress'es are absolute, this is always legal.
-- Note, we could wrap, and that isn't really checked.
addressAddOffset :: (MM.MemWidth w)
                 => MM.Memory w
                 -> ConcreteAddress w
                 -> MM.MemWord w
                 -> ConcreteAddress w
addressAddOffset _mem (ConcreteAddress memAddr) memWord =
  ConcreteAddress (MM.incAddr (fromIntegral memWord) memAddr)

-- | Compute the distance between two 'ConcreteAddress'es
--
-- This is actually total because all of our 'ConcreteAddress'es are absolute
-- (and thus have the same base), so the difference is always valid.
addressDiff :: (MM.MemWidth w) => ConcreteAddress w -> ConcreteAddress w -> Integer
addressDiff (ConcreteAddress memAddr1) (ConcreteAddress memAddr2) = diff
  where
    Just diff = MM.diffAddr memAddr1 memAddr2
