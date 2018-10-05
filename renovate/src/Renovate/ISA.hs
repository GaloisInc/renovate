{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | This module defines the interface required for describing an 'ISA'
-- to the rewriter.
--
-- Implementations of 'ISA's are in separate @renovate-<arch>@ packages.
module Renovate.ISA (
  ISA(..),
  JumpType(..),
  JumpCondition(..),
  TrapPredicate(..)
  ) where

import Data.Word ( Word8, Word64 )

import qualified Data.Macaw.CFG as MM

import Renovate.Address
import Renovate.BasicBlock.Types ( Instruction, InstructionAnnotation, TaggedInstruction )

-- | The variety of a jump: either conditional or unconditional.  This
-- is used as a tag for 'JumpType's.  One day, we could model the type
-- of condition here if it began to matter.
data JumpCondition = Unconditional
                   | Conditional
                   deriving (Show, Eq)

-- | Metadata about jump instructions
--
-- Note that we model calls as conditional jumps.  That isn't exactly
-- right, but it captures the important aspect of calls for basic
-- block recovery: execution continues after the return.
data JumpType arch = RelativeJump JumpCondition (ConcreteAddress arch) (MM.MemWord (MM.ArchAddrWidth arch))
                -- ^ A relative jump by some offset in bytes, which
                -- could be negative.  The 'Address' is the address
                -- from which the jump was issued.
                | AbsoluteJump JumpCondition (ConcreteAddress arch)
                -- ^ A jump to an absolute address
                | IndirectJump JumpCondition
                -- ^ A jump type for indirect jumps, which end blocks
                -- but do not let us find new code.
                | DirectCall (ConcreteAddress arch) (MM.MemWord (MM.ArchAddrWidth arch))
                -- ^ A call to a known location expressed as an offset
                -- from the jump location (note, this might be
                -- difficult to fill in for RISC architectures - macaw
                -- would be better suited to finding this information)
                | IndirectCall
                -- ^ A call to an unknown location
                | Return
                | NoJump
                -- ^ The instruction is not a jump
                deriving (Eq)

deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (JumpType arch)

-- | Information about an ISA.
--
-- The @i@ type parameter is the underlying instruction type, which
-- accepts an annotation parameter.
--
-- The @a@ type parameter is the type of the annotations of /symbolic/
-- instructions, and contains information to link control flow
-- transfer instructions to their symbolic targets.  The information
-- required can vary by ISA, so this is a parameter.
--
-- Concrete instructions have @()@ as their annotation.
--
-- The functions `isaSymbolizeAddress` and `isaConcretizeAddress`
-- convert between concrete and symbolic instructions.
--
-- See separate @renovate-<arch>@ packages for actual 'ISA'
-- definitions.
data ISA arch =
  ISA { isaInstructionSize :: forall t . Instruction arch t -> Word8
        -- ^ Compute the size of an instruction in bytes
      , isaSymbolizeAddresses :: MM.Memory (MM.ArchAddrWidth arch)
                              -> (ConcreteAddress arch -> Maybe (SymbolicAddress arch))
                              -> ConcreteAddress arch
                              -> Maybe (SymbolicAddress arch)
                              -> Instruction arch ()
                              -> [TaggedInstruction arch (InstructionAnnotation arch)]
        -- ^ Abstract instructions and annotate them. The contract is that this
        -- function can change the opcode, but the selected instruction must
        -- never change sizes later (during concretization). That is, for all
        -- concrete addresses, the chosen instruction must have the same size.
        --
        -- * The 'ConcreteAddress' is the address of the instruction
        --
        -- * The 'SymbolicAddress' (if any) is the direct jump target (possibly conditional) if any
        --
        -- NOTE: This function is allowed to return larger instructions now and,
        -- in fact, may return extra instructions.
      , isaConcretizeAddresses :: MM.Memory (MM.ArchAddrWidth arch) -> ConcreteAddress arch -> Instruction arch (InstructionAnnotation arch) -> Instruction arch ()
        -- ^ Remove the annotation, with possible post-processing.
      , isaJumpType :: forall t . Instruction arch t -> MM.Memory (MM.ArchAddrWidth arch) -> ConcreteAddress arch -> JumpType arch
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
      , isaMakeRelativeJumpTo :: ConcreteAddress arch -> ConcreteAddress arch -> [Instruction arch ()]
        -- ^ Create a relative jump from the first 'ConcreteAddress'
        -- to the second.  This will call error if the range is too
        -- far (probably more than 2GB).
      , isaModifyJumpTarget :: Instruction arch () -> ConcreteAddress arch -> ConcreteAddress arch -> Maybe (Instruction arch ())
        -- ^ Modify the given jump instruction, rather than creating
        -- an entirely new one.  This differs from
        -- 'isaMakeRelativeJumpTo' in that it preserves the jump type
        -- (e.g., the type of conditional jump).
        --
        -- NOTE: This function must not change the size of the instruction, as
        -- it is called after code layout is done, so we cannot re-arrange
        -- anything.
      , isaMakePadding :: Word64 -> [Instruction arch ()]
        -- ^ Make the given number of bytes of padding instructions.
        -- The semantics of the instruction stream should either be
        -- no-ops or halts (i.e., not meant to be executed).
      , isaMakeTrapIf :: forall t. Instruction arch t -> TrapPredicate -> [Instruction arch (InstructionAnnotation arch)]
        -- ^ Create an instruction sequence that halts if the given
        -- instruction meets the given predicate.  For example, it
        -- could create a conditional halt if the instruction created
        -- a signed overflow.
      , isaMakeSymbolicJump :: SymbolicAddress arch -> [TaggedInstruction arch (InstructionAnnotation arch)]
      -- ^ Make an unconditional jump that takes execution to the given symbolic
      -- target.
      , isaPrettyInstruction :: forall t . Instruction arch t -> String
      -- ^ Pretty print an instruction for diagnostic purposes
      }

-- | Predicates supported for 'isaMakeTrapIf'
data TrapPredicate = SignedOverflow
                   | UnsignedOverflow
                   deriving (Eq, Ord, Show)
{-

With the jump type test, we probably want to make a distinction
between intra-segment jumps and inter-segment jumps.  We can rewrite
the former.  The latter are best left alone for now... though shared
libraries will make that interesting.

-}
