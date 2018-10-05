{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
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
import qualified Data.Macaw.CFG as MM


-- | Symbolic addresses that can be referenced abstractly and
-- relocated.  They will automatically be concretized when blocks are
-- laid out.
--
-- The 'Word64' value of the 'SymbolicAddress' is a unique identifier.
--
--  * 'SymbolicAddress' denotes a referenced address to a code entity that
--    could be relocated.  In this case, both the source of the reference and
--    the target of the reference can be relocated
--  * 'StableAddress' denotes a reference to a code entity that cannot be relocated
--    (e.g., because it is outside of the text section).  In this case, the source
--    of the reference may be relocated (so a relative offset may need to be recomputed)
--    but the target of the reference will not be relocated.
data SymbolicAddress arch = SymbolicAddress Word64
                          | StableAddress (ConcreteAddress arch)
                          deriving (Eq, Ord)

deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (SymbolicAddress arch)

instance (MM.MemWidth (MM.ArchAddrWidth arch)) => PD.Pretty (SymbolicAddress arch) where
  pretty (SymbolicAddress a) = "0x" PD.<> PD.pretty (N.showHex a "")
  pretty (StableAddress a) = PD.pretty a

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
newtype ConcreteAddress arch = ConcreteAddress (MM.MemAddr (MM.ArchAddrWidth arch))
  deriving (Eq, Ord)

deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (ConcreteAddress arch)

instance (MM.MemWidth (MM.ArchAddrWidth arch)) => PD.Pretty (ConcreteAddress arch) where
  pretty (ConcreteAddress memAddr) = PD.pretty (show memAddr)

-- | Construct a 'ConcreteAddress' from a 'MM.MemSegmentOff'
--
-- This can fail if the 'MM.MemSegmentOff' is not an absolute address (i.e., it
-- is a pointer to a location in another module)
concreteFromSegmentOff :: (L.HasCallStack, MM.MemWidth (MM.ArchAddrWidth arch))
                       => MM.Memory (MM.ArchAddrWidth arch)
                       -> MM.MemSegmentOff (MM.ArchAddrWidth arch)
                       -> Maybe (ConcreteAddress arch)
concreteFromSegmentOff _mem segOff = do
  let memAddr = MM.relativeSegmentAddr segOff
  _ <- MM.asAbsoluteAddr memAddr
  return (ConcreteAddress memAddr)

concreteAsSegmentOff :: (MM.MemWidth (MM.ArchAddrWidth arch)) => MM.Memory (MM.ArchAddrWidth arch) -> ConcreteAddress arch -> Maybe (MM.MemSegmentOff (MM.ArchAddrWidth arch))
concreteAsSegmentOff mem (ConcreteAddress memAddr) = MM.asSegmentOff mem memAddr

-- | Construct a 'ConcreteAddress' from a concrete address specified as a
-- 'MM.MemWord'
--
-- This always succeeds, as we just treat a number as an absolute address.
--
-- This function does not ensure that the address is mapped in the underlying
-- 'MM.Memory'.
concreteFromAbsolute :: (MM.MemWidth (MM.ArchAddrWidth arch))
                     => MM.MemWord (MM.ArchAddrWidth arch)
                     -> ConcreteAddress arch
concreteFromAbsolute = ConcreteAddress . MM.absoluteAddr

-- | Convert a 'ConcreteAddress' into an absolute address (a 'MM.MemWord')
--
-- This always succeeds, because our 'ConcreteAddress' is a wrapper that
-- guarantees we only have absolute addresses.
absoluteAddress :: (MM.MemWidth (MM.ArchAddrWidth arch)) => ConcreteAddress arch -> MM.MemWord (MM.ArchAddrWidth arch)
absoluteAddress (ConcreteAddress memAddr) = absAddr
  where
    Just absAddr = MM.asAbsoluteAddr memAddr
{-# INLINE absoluteAddress #-}

-- | Add an offset to a 'ConcreteAddress'
--
-- Since all of our 'ConcreteAddress'es are absolute, this is always legal.
-- Note, we could wrap, and that isn't really checked.
addressAddOffset :: (MM.MemWidth (MM.ArchAddrWidth arch))
                 => ConcreteAddress arch
                 -> MM.MemWord (MM.ArchAddrWidth arch)
                 -> ConcreteAddress arch
addressAddOffset (ConcreteAddress memAddr) memWord =
  ConcreteAddress (MM.incAddr (fromIntegral memWord) memAddr)

-- | Compute the distance between two 'ConcreteAddress'es
--
-- This is actually total because all of our 'ConcreteAddress'es are absolute
-- (and thus have the same base), so the difference is always valid.
addressDiff :: (MM.MemWidth (MM.ArchAddrWidth arch)) => ConcreteAddress arch -> ConcreteAddress arch -> Integer
addressDiff (ConcreteAddress memAddr1) (ConcreteAddress memAddr2) = diff
  where
    Just diff = MM.diffAddr memAddr1 memAddr2
