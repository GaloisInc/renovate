{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
-- | An abstract interface for 'ABI's.
--
-- There can be more than one 'ABI' for a given 'ISA'.
--
-- The implementations of 'ABI's will be under the Renovate.Arch module
-- hierarchy.
module Renovate.ABI (
  ABI(..)
  ) where

import Data.Word ( Word8, Word32 )
import Renovate.Address
import Renovate.BasicBlock

-- | A description of the properties of an Application Binary
-- Interface (ABI) that we currently care about.  Right now, it is
-- information about registers and their roles in returns.
--
-- The type parameter 'i' is the instruction type, and 'r' is the
-- register type.
data ABI arch =
  ABI { isReturn :: forall t (tp :: InstructionArchReprKind arch) . Instruction arch tp t -> Bool
      -- ^ Return 'True' if the given instruction is a return from a
      -- function.  Note that we will need to extend this somehow to
      -- be able to look at context or use more information from the
      -- code recovery phase to more accurately find returns.
      , pointerSize :: Word8
      -- ^ Number of bytes in a pointer
      , callerSaveRegisters :: forall tp . InstructionArchRepr arch tp -> [RegisterType arch tp]
      -- ^ The list of caller-save registers for this ABI
      , clearRegister :: forall tp . InstructionArchRepr arch tp -> RegisterType arch tp -> Instruction arch tp (Relocation arch)
      -- ^ Create an instruction to clear a register (i.e., set it to
      -- zero or some other distinguished neutral value).
      , allocateMemory :: forall tp . InstructionArchRepr arch tp -> Word32 -> ConcreteAddress arch -> [Instruction arch tp (Relocation arch)]
      -- ^ Generate a list of instructions that allocate a known (at
      -- rewriting time) number of bytes in the heap and store the
      -- address of the allocated memory block in the provided
      -- address.
      , computeStackPointerOffset :: forall tp . InstructionArchRepr arch tp -> ConcreteAddress arch -> [Instruction arch tp (Relocation arch)]
      -- ^ Take the value at the address and subtract the stack
      -- pointer from it, storing the result back in the given
      -- address.  It changes the type of the value at the address
      -- from a pointer to an offset from the stack pointer.
      , saveReturnAddress :: forall tp . InstructionArchRepr arch tp  -> ConcreteAddress arch -> [Instruction arch tp (Relocation arch)]
      -- ^ Save the return address at an offset from its location on
      -- the stack.  The offset is held in the memory address provided
      -- as the first argument.  Note that this must be installed as
      -- the first code in the function entry, otherwise it will store
      -- garbage.  That restriction could be relaxed if we had frame
      -- pointers, but we can't really count on that.
      , checkShadowStack :: forall tp . InstructionArchRepr arch tp -> ConcreteAddress arch -> [Instruction arch tp (Relocation arch)]
      -- ^ Check the value on the shadow stack against the return
      -- address on the top of the stack.  Fault if they differ.  The
      -- offset of the shadow stack from the real stack is stored at
      -- the memory address provided.
      --
      -- As in 'saveReturnAddress', this must be placed right before
      -- the return instruction, otherwise there might be some junk on
      -- the stack.  Note that we are free to use caller-save
      -- registers at that point without preserving them.
      }

