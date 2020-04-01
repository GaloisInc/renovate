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
  ) where

import qualified Data.List.NonEmpty as DLN
import           Data.Word ( Word8, Word64 )

import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Types as MT

import qualified Renovate.Address as RA
import           Renovate.BasicBlock.Types

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
  NotInstrumentable :: RA.ConcreteAddress arch -> JumpType arch NoModifiableTarget

deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (JumpType arch k)
deriving instance Eq (JumpType arch k)

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
-- Concrete instructions have @()@ as their annotation.
--
-- The functions `isaSymbolizeAddress` and `isaConcretizeAddress`
-- convert between concrete and symbolic instructions.
--
-- See separate @renovate-<arch>@ packages for actual 'ISA'
-- definitions.
data ISA arch = ISA
  { isaInstructionSize :: forall t tp . Instruction arch tp t -> Word8
    -- ^ Compute the size of an instruction in bytes
  , isaSymbolizeAddresses :: forall tp
                           . MM.Memory (MM.ArchAddrWidth arch)
                          -> RA.ConcreteAddress arch
                          -> Maybe (RA.SymbolicAddress arch)
                          -> Instruction arch tp ()
                          -> [TaggedInstruction arch tp (InstructionAnnotation arch)]
    -- ^ Abstract instructions and annotate them.
    --
    -- * The 'RA.ConcreteAddress' is the address of the instruction
    --
    -- * The 'RA.SymbolicAddress' (if any) is the direct jump target (possibly conditional) if any
    --
    -- NOTE: This function is allowed to return larger instructions now and,
    -- in fact, may return extra instructions. We also now allow the
    -- concretization phase to increase the size of the instructions, provided
    -- that concretizing to targets that are 'isaMaxRelativeJumpSize' far away
    -- has the worst case size behavior.
  , isaConcretizeAddresses :: forall tp
                            . MM.Memory (MM.ArchAddrWidth arch)
                           -> RA.ConcreteAddress arch
                           -> Instruction arch tp (InstructionAnnotation arch)
                           -> Instruction arch tp ()
    -- ^ Remove the annotation, with possible post-processing.
  , isaJumpType :: forall t tp ids
                 . Instruction arch tp t
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
                          -> InstructionArchRepr arch tp
                          -> DLN.NonEmpty (Instruction arch tp ())
    -- ^ Create a relative jump from the first 'RA.ConcreteAddress'
    -- to the second.  This will call error if the range is too
    -- far (see 'isaMaxRelativeJumpSize').
  , isaMaxRelativeJumpSize :: forall tp . InstructionArchRepr arch tp -> Word64
    -- ^ How far can this architecture's unconditional relative jumps reach?
    -- New code blocks will be laid out in virtual address space within this
    -- many bytes of the original code blocks, so that the two can jump to each
    -- other as necessary.
  , isaModifyJumpTarget :: forall tp
                         . RA.ConcreteAddress arch
                        -> Instruction arch tp ()
                        -> RelocatableTarget arch RA.ConcreteAddress HasSomeTarget
                        -> Maybe (DLN.NonEmpty (Instruction arch tp ()))
    -- ^ Modify the given jump instruction, accounting for any fallthrough behavior specified
    --
    -- The first argument is the address of the original jump instruction
    --
    -- The second argument is the jump instruction itself
    --
    -- The third argument is evidence that the instruction is actually one that
    -- can have its target modified.  NOTE: There is currently no formal
    -- connection between the instruction and the jump type, so a caller could
    -- pass in a manually-constructed jump type to circumvent the protection.
    --
    -- The fourth argument is the concretized new target
  , isaDefaultInstructionArchRepr :: SomeInstructionArchRepr arch
  -- ^ The default arch repr to use if there is nothing else, used in particular
  -- in the creation of padding.
  , isaMakePadding :: forall tp . Word64 -> InstructionArchRepr arch tp -> [Instruction arch tp ()]
    -- ^ Make the given number of bytes of padding instructions.
    -- The semantics of the instruction stream should either be
    -- no-ops or halts (i.e., not meant to be executed).
  , isaMakeSymbolicJump
      :: forall tp
       . RA.SymbolicAddress arch
      -> InstructionArchRepr arch tp
      -> [TaggedInstruction arch tp (InstructionAnnotation arch)]
  -- ^ Make an unconditional jump that takes execution to the given symbolic
  -- target.
  , isaMakeSymbolicCall
      :: forall tp
       . RA.SymbolicAddress arch
      -> TaggedInstruction arch tp (InstructionAnnotation arch)
  -- ^ Make an call that takes execution to the given symbolic target.
  , isaPrettyInstruction :: forall t tp. Instruction arch tp t -> String
  -- ^ Pretty print an instruction for diagnostic purposes
  , isaMove
      :: forall tp
       . Some MT.TypeRepr
      -> InstructionArchRepr arch tp
      -> RegisterType arch tp
      -> RegisterType arch tp
      -> Instruction arch tp (InstructionAnnotation arch)
  , isaMoveImmediate
      :: forall tp
       . Some MT.TypeRepr
      -> InstructionArchRepr arch tp
      -> RegisterType arch tp
      -> Integer
      -> Instruction arch tp (InstructionAnnotation arch)
  , isaLoad
      :: forall tp
       . Some MT.TypeRepr
      -> InstructionArchRepr arch tp
      -> RegisterType arch tp
      -> StackAddress arch tp
      -> Instruction arch tp (InstructionAnnotation arch)
  , isaStore
      :: forall tp
       . Some MT.TypeRepr
      -> InstructionArchRepr arch tp
      -> StackAddress arch tp
      -> RegisterType arch tp
      -> Instruction arch tp (InstructionAnnotation arch)
  , isaStoreImmediate
      :: forall tp
       . Some MT.TypeRepr
      -> InstructionArchRepr arch tp
      -> StackAddress arch tp
      -> Integer
      -> Instruction arch tp (InstructionAnnotation arch)
  , isaAddImmediate
      :: forall tp
       . RegisterType arch tp
      -> Integer
      -> [Instruction arch tp (InstructionAnnotation arch)]
  , isaSubtractImmediate
      :: forall tp
       . RegisterType arch tp
      -> Integer
      -> [Instruction arch tp (InstructionAnnotation arch)]
  }

data StackAddress arch (tp :: InstructionArchReprKind arch) = StackAddress
  { saBase :: RegisterType arch tp
  , saOffset :: Integer
  }

deriving instance Eq (RegisterType arch tp) => Eq (StackAddress arch tp)
deriving instance Ord (RegisterType arch tp) => Ord (StackAddress arch tp)
deriving instance Show (RegisterType arch tp) => Show (StackAddress arch tp)

{-

With the jump type test, we probably want to make a distinction
between intra-segment jumps and inter-segment jumps.  We can rewrite
the former.  The latter are best left alone for now... though shared
libraries will make that interesting.

-}
