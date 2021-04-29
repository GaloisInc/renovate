{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Renovate.Core.Relocation (
    ArchitectureRelocation
  , Relocation(..)
  ) where

import           Data.Kind ( Type )
import qualified Data.Macaw.CFG as MC

import qualified Renovate.Core.Address as RCA

-- | An architecture-specific relocation extension type
--
-- If this is not needed, instantiate it as 'Void'
type family ArchitectureRelocation arch :: Type

-- | Representations of relocations that must be resolved during instruction concretization
--
-- Each operand is annotated with a relocation that contains enough information
-- to be resolved during the concretization phase of rewriting. These
-- relocations are analogous to those in object files, and represent symbolic
-- addresses that must be concretized.
data Relocation arch where
  -- | A reference to an absolute address by means of a PC-relative offset that
  -- needs to be re-computed when the PC of the instruction changes
  --
  -- These are largely used for referencing existing data values in a
  -- position-independent way, as data is never assigned a symbolic address (as
  -- moving existing data is too dangerous in general)
  PCRelativeRelocation :: RCA.ConcreteAddress arch -> Relocation arch
  -- | A reference to a symbolic location that should be referenced (probably by
  -- a PC-relative offset) once both the containing instruction and target have
  -- been assigned addresses.
  --
  -- These are assigned to injected code, injected data, and code that may move
  -- (e.g., jump targets)
  SymbolicRelocation :: RCA.SymbolicAddress arch -> Relocation arch
  -- | An architecture-specific relocation type
  --
  -- If this is not needed, instantiate the 'ArchitectureRelocation' type as 'Void'
  ArchRelocation :: ArchitectureRelocation arch -> Relocation arch
  -- | For operands that do not require relocations
  NoRelocation :: Relocation arch

deriving instance (Show (ArchitectureRelocation arch), MC.MemWidth (MC.ArchAddrWidth arch)) => Show (Relocation arch)
