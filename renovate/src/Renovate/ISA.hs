{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
-- | This module defines the interface required for describing an 'ISA'
-- to the rewriter.
--
-- Implementations of 'ISA's are in separate @renovate-<arch>@ packages.
module Renovate.ISA
  ( ISA(..)
  , JumpType(..)
  , JumpCondition(..)
  , StackAddress(..)
  , HasModifiableTarget
  , NoModifiableTarget
  , isaDefaultInstructionArchRepr
  , ArchConstraints
  ) where

import qualified Data.List.NonEmpty as DLN
import           Data.Word ( Word8, Word64 )

import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.Discovery as MD
import qualified Data.Parameterized.Classes as PC
import           Data.Parameterized.Some ( Some(..) )

import qualified Renovate.Core.Address as RA
import qualified Renovate.Core.Instruction as RCI
import qualified Renovate.Core.Relocation as RCR

-- | Constraints that must be available for an architecture
--
-- Note that this differs from 'RCI.InstructionConstraints' in that these
-- constraints do not (and cannot) mention the @tp@ type parameter marking the
-- sub-ISA.
type ArchConstraints arch =
  ( PC.OrdF (RCI.RegisterType arch)
  , MM.MemWidth (MM.ArchAddrWidth arch)
  , PC.TestEquality (RCI.InstructionArchRepr arch)
  , PC.OrdF (RCI.InstructionArchRepr arch)
  )

-- | The variety of a jump: either conditional or unconditional.  This
-- is used as a tag for 'JumpType's.  One day, we could model the type
-- of condition here if it began to matter.
data JumpCondition = Unconditional
                   | Conditional
                   deriving (Show, Eq)

data JumpKind = HasModifiableTarget
              | NoModifiableTarget

-- | A tag denoting a jump type where we can modify the target by modifying the
-- instruction (i.e., where the target is encoded as an immediate operand in
-- some way, either as an address or an offset)
type HasModifiableTarget = 'HasModifiableTarget
-- | A tag denoting a jump that does not have a target that we can modify by
-- changing an operand
type NoModifiableTarget = 'NoModifiableTarget

-- | Metadata about jump instructions
--
-- Note that we model calls as conditional jumps.  That isn't exactly
-- right, but it captures the important aspect of calls for basic
-- block recovery: execution continues after the return.
data JumpType arch k where
  -- | A relative jump by some offset in bytes, which could be negative.  The
  -- 'RA.ConcreteAddress' is the address from which the jump was issued.
  RelativeJump :: JumpCondition -> RA.ConcreteAddress arch -> MM.MemWord (MM.ArchAddrWidth arch) -> JumpType arch HasModifiableTarget
  -- | A jump to an absolute address
  AbsoluteJump :: JumpCondition -> RA.ConcreteAddress arch -> JumpType arch HasModifiableTarget
  -- | A jump type for indirect jumps, which end blocks but do not let us find
  -- new code.
  IndirectJump :: JumpCondition -> JumpType arch NoModifiableTarget
  -- | A call to a known location expressed as an offset from the jump location
  -- (note, this might be difficult to fill in for RISC architectures - macaw
  -- would be better suited to finding this information)
  DirectCall :: RA.ConcreteAddress arch -> MM.MemWord (MM.ArchAddrWidth arch) -> JumpType arch HasModifiableTarget
  -- | A call to an unknown location
  IndirectCall :: JumpType arch NoModifiableTarget
  -- | A possibly conditional return
  Return :: JumpCondition -> JumpType arch NoModifiableTarget
  -- | The instruction is not a jump
  NoJump :: JumpType arch NoModifiableTarget
  -- | The instruction is a recognized control flow transfer, but not one that
  -- can be rewritten (and thus is not permitted to be instrumented)
  --
  -- The address is the address of the instruction
  NotInstrumentable :: RA.ConcreteAddress arch -> JumpType arch NoModifiableTarget

deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (JumpType arch k)
deriving instance Eq (JumpType arch k)

instance (MM.MemWidth (MM.ArchAddrWidth arch)) => PC.ShowF (JumpType arch)

-- | Information about an ISA.
--
-- The @Instruction arch@ type family is the underlying instruction
-- type, which accepts an annotation parameter.
--
-- The @InstructionAnnotation arch@ type family is the type of the
-- annotations of /symbolic/ instructions, and contains information to
-- link control flow transfer instructions to their symbolic targets.
-- The information required can vary by ISA, so this is a parameter.
--
-- Concrete instructions have @()@ as their annotation, while symbolic
-- instructions have 'R.Relocation's as their annotation.
--
-- The functions `isaSymbolizeAddress` and `isaConcretizeAddress`
-- convert between concrete and symbolic instructions.
--
-- See separate @renovate-<arch>@ packages for actual 'ISA'
-- definitions.
data ISA arch = ISA
  { isaInstructionSize :: forall t tp . RCI.Instruction arch tp t -> Word8
    -- ^ Compute the size of an instruction in bytes
  , isaInstructionRepr :: forall t tp . RCI.Instruction arch tp t -> RCI.InstructionArchRepr arch tp
    -- ^ Return the type representative for an instruction
  , isaSymbolizeAddresses :: forall tp ids
                           . MM.Memory (MM.ArchAddrWidth arch)
                          -> (RA.ConcreteAddress arch -> RA.SymbolicAddress arch)
                          -> MD.ParsedBlock arch ids
                          -> RA.ConcreteAddress arch
                          -> RCI.Instruction arch tp ()
                          -> [RCI.Instruction arch tp (RCR.Relocation arch)]
    -- ^ Abstract instructions and annotate them with relocations
    --
    -- * The function converts concrete (absolute) addresses into symbolic addresses, which will be wrapped up in relocations
    --
    -- * The 'RA.ConcreteAddress' is the address of the instruction
    --
    -- NOTE: This function is allowed to return larger instructions now and,
    -- in fact, may return extra instructions. We also now allow the
    -- concretization phase to increase the size of the instructions, provided
    -- that concretizing to targets that are 'isaMaxRelativeJumpSize' far away
    -- has the worst case size behavior.
  , isaConcretizeAddresses :: forall tp
                            . MM.Memory (MM.ArchAddrWidth arch)
                           -> (RA.SymbolicAddress arch -> RA.ConcreteAddress arch)
                           -> RA.ConcreteAddress arch
                           -> RCI.Instruction arch tp (RCR.Relocation arch)
                           -> DLN.NonEmpty (RCI.Instruction arch tp ())
    -- ^ Remove the annotation, with possible post-processing.
    --
    -- This is intended to fix up PC-relative memory references in operands and
    -- modify relative jump targets to point to the proper locations.
    --
    -- NOTE: It is allowed to return extra instructions to accomplish these things.
  , isaJumpType :: forall t tp ids
                 . RCI.Instruction arch tp t
                -> MM.Memory (MM.ArchAddrWidth arch)
                -> RA.ConcreteAddress arch
                -> MD.ParsedBlock arch ids
                -> Some (JumpType arch)
    -- ^ Test if an instruction is a jump; if it is, return some
    -- metadata about the jump (destination or offset).
    --
    -- The 'MC.Memory' parameter is the memory space containing
    -- the known code region.  Jumps outside of the known code
    -- region are treated as library code and are not
    -- followed.
    --
    -- The 'Address' parameter is the address of the instruction,
    -- which is needed to resolve relative jumps.
  , isaMakeRelativeJumpTo :: forall tp
                           . RA.ConcreteAddress arch
                          -> RA.ConcreteAddress arch
                          -> RCI.InstructionArchRepr arch tp
                          -> DLN.NonEmpty (RCI.Instruction arch tp ())
    -- ^ Create a relative jump from the first 'RA.ConcreteAddress'
    -- to the second.  This will call error if the range is too
    -- far (see 'isaMaxRelativeJumpSize').
  , isaMaxRelativeJumpSize :: forall tp . RCI.InstructionArchRepr arch tp -> Word64
    -- ^ How far can this architecture's unconditional relative jumps reach?
    -- New code blocks will be laid out in virtual address space within this
    -- many bytes of the original code blocks, so that the two can jump to each
    -- other as necessary.
  , isaInstructionArchReprs :: DLN.NonEmpty (RCI.SomeInstructionArchRepr arch)
  -- ^ The default arch repr to use if there is nothing else, used in particular
  -- in the creation of padding.
  , isaMakePadding :: forall tp . Word64 -> RCI.InstructionArchRepr arch tp -> [RCI.Instruction arch tp ()]
    -- ^ Make the given number of bytes of padding instructions.
    -- The semantics of the instruction stream should either be
    -- no-ops or halts (i.e., not meant to be executed).
  , isaPrettyInstruction :: forall t tp. RCI.Instruction arch tp t -> String
  -- ^ Pretty print an instruction for diagnostic purposes
  }

data StackAddress arch (tp :: RCI.InstructionArchReprKind arch) = StackAddress
  { saBase :: RCI.RegisterType arch tp
  , saOffset :: Integer
  }

deriving instance Eq (RCI.RegisterType arch tp) => Eq (StackAddress arch tp)
deriving instance Ord (RCI.RegisterType arch tp) => Ord (StackAddress arch tp)
deriving instance Show (RCI.RegisterType arch tp) => Show (StackAddress arch tp)

isaDefaultInstructionArchRepr :: ISA arch -> RCI.SomeInstructionArchRepr arch
isaDefaultInstructionArchRepr isa = DLN.head (isaInstructionArchReprs isa)

{-

With the jump type test, we probably want to make a distinction
between intra-segment jumps and inter-segment jumps.  We can rewrite
the former.  The latter are best left alone for now... though shared
libraries will make that interesting.

-}
