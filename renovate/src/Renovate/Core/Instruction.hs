{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Renovate.Core.Instruction (
    InstructionArchReprKind
  , InstructionArchRepr
  , RegisterType
  , SomeInstructionArchRepr(..)
  , Instruction
  , ToGenericInstruction(..)
  , InstructionConstraints
  ) where

import           Data.Kind ( Type )
import qualified Data.Parameterized.Classes as PC
import qualified Prettyprinter as PP

import qualified Data.Macaw.CFG as MC
import qualified SemMC.Architecture as SA

-- | The kind of instruction arch reprs
--
-- This is a type family because each architecture-specific backend will provide
-- its own type here.
type family InstructionArchReprKind arch :: k

-- | A value-level representative of the type of instructions in a basic block
--
-- The intent of this is to support architectures with multiple instruction sets
-- (e.g., AArch32 with ARM and Thumb).  This lets us enforce that each basic
-- block has instructions from only a single architecture.
type family InstructionArchRepr arch :: InstructionArchReprKind arch -> Type

-- | The type of register values for the architecture
type family RegisterType arch :: InstructionArchReprKind arch -> Type

-- | Constraints common to all instructions.
--
-- All 'Instruction' instances must be 'Show'able and 'Typeable'.  They are
-- combined for convenience and to reduce noise in type signatures, since those
-- constraints are not very interesting.
type InstructionConstraints arch (tp :: InstructionArchReprKind arch) =
  ( PP.Pretty (Instruction arch tp ())
  , Show (Instruction arch tp ())
  , Eq (Instruction arch tp ())
  , PC.TestEquality (InstructionArchRepr arch)
  , PC.OrdF (InstructionArchRepr arch)
  , Eq (RegisterType arch tp)
  , Ord (RegisterType arch tp)
  , PC.OrdF (RegisterType arch)
  , MC.MemWidth (MC.ArchAddrWidth arch)
  )

-- | An existential wrapper for instruction arch reprs that captures some needed constraints
data SomeInstructionArchRepr arch where
  SomeInstructionArchRepr :: ( InstructionConstraints arch tp )
                          => InstructionArchRepr arch tp
                          -> SomeInstructionArchRepr arch

-- | The type of instructions for an architecture
--
-- Instructions are parameterized by an annotation type that is usually either
-- '()' or @'InstructionAnnotation' arch@
type family Instruction arch :: InstructionArchReprKind arch -> Type -> Type

-- | Concrete renovate instructions of the type @'Instruction' arch ()@ are in
-- several cases equivalent to semmc instructions of type
-- @'SemMC.Architecture.Instruction' arch@. This property is not true for X86
-- instructions, but is true for PowerPC.
class ToGenericInstruction arch
  where
    toGenericInstruction   :: forall (tp :: InstructionArchReprKind arch) a . Instruction arch tp a -> SA.Instruction arch
    fromGenericInstruction :: forall (tp :: InstructionArchReprKind arch) . InstructionArchRepr arch tp -> SA.Instruction arch -> Instruction arch tp ()
