{-
Module           : Renovate.BinaryFormat.ELF.Common.Internal
Description      : Common operations for dealing with ELF files
Copyright        : (c) Galois, Inc 2020
License          : BSD3
Maintainer       : Langston Barrett <langston@galois.com>
Stability        : provisional

This module is exposed for testing purposes only. Do not import outside of
Renovate!
-}


module Renovate.BinaryFormat.ELF.Common.Internal
  ( alignValue
  , alignValueDown
  ) where

-- | Align a value
--
-- @alignValue v alignment@ returns the value @v'@ greater than or equal to @v@
-- such that @v' % align == 0@.
--
-- For an alignment of zero or one, return @v@.
alignValue :: (Integral w) => w -> w -> w
alignValue v 0 = v
alignValue v alignment = v + ((alignment - (v `mod` alignment)) `mod` alignment)

-- | Align a value downwards
--
-- @alignValueDown v alignment@ returns the value @v'@ less than or equal to @v@
-- such that @v' % align == 0@.
--
-- For an alignment of zero or one, return @v@.
alignValueDown :: (Integral w) => w -> w -> w
alignValueDown v 0 = v
alignValueDown v alignment = v - (v `mod` alignment)
