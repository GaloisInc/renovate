{-# LANGUAGE ScopedTypeVariables, TypeApplications, DataKinds, GADTs #-}

-- | An 'ABI' implementation for the PPC ABI

module Renovate.Arch.PPC.ABI ( abi64
                             , abi32
                             , instrOpcode
                             ) where

import qualified Data.Macaw.PPC as PPC
import qualified Dismantle.PPC  as D
import qualified Renovate       as R
import           Renovate.Arch.PPC.ISA

import qualified Data.Parameterized.Some as Some

-- TODO: some of this could be moved into an Internals.hs

-- | Note that allocateMemory, computeStackPointerOffset, saveReturnaddress, and
-- checkShadowStack, are currently used only for the Shadow stack embrittle
-- transforamtion, which we are not applying on PPC right now. As a result, we
-- leave these undefined for the time being.
abi64 :: R.ABI PPC.PPC64
abi64 = R.ABI { R.isReturn            = ppcIsReturn . R.toGenericInstruction @PPC.PPC64 
            , R.callerSaveRegisters = ppcCallerSaveRegisters64
            , R.clearRegister       = fmap (const NoAddress) 
                                    . R.fromGenericInstruction @PPC.PPC64 
                                    . ppcClearRegister
            , R.pointerSize         = 8
            -- | these are all for shadow stack; leave undefined
            , R.allocateMemory            = undefined
            , R.computeStackPointerOffset = undefined
            , R.saveReturnAddress         = undefined
            , R.checkShadowStack          = undefined
            }

-- | Note that allocateMemory, computeStackPointerOffset, saveReturnaddress, and
-- checkShadowStack, are currently used only for the Shadow stack embrittle
-- transforamtion, which we are not applying on PPC right now. As a result, we
-- leave these undefined for the time being.
abi32 :: R.ABI PPC.PPC32
abi32 = R.ABI { R.isReturn            = ppcIsReturn . R.toGenericInstruction @PPC.PPC32
            , R.callerSaveRegisters = ppcCallerSaveRegisters32
            , R.clearRegister       = fmap (const NoAddress) 
                                    . R.fromGenericInstruction @PPC.PPC32
                                    . ppcClearRegister
            , R.pointerSize         = 4
            -- | these are all for shadow stack; leave undefined
            , R.allocateMemory            = undefined
            , R.computeStackPointerOffset = undefined
            , R.saveReturnAddress         = undefined
            , R.checkShadowStack          = undefined
            }


-- | Retrieve the 'String' opcode from an instruction (for diagnostic purposes)
instrOpcode :: D.Instruction -> String
instrOpcode (D.Instruction opcode operands) = show opcode

-- is return if it matches BLR (branch to link register) or BLRL 
ppcIsReturn :: D.Instruction -> Bool
ppcIsReturn instr = instrOpcode instr == "BLR" || instrOpcode instr == "BLRL"


-- | Caller save registers for PPC64 (called "volitile" in the
-- [documentation]<https://gitlab-int.galois.com/brittle/sfe/uploads/5bfc68a341709773ff8cb24552ece62b/PPC-elf64abi-1.7.pdf>)
--
-- For now we are only accounting for general-purpose registers @r4@-@r10@.
ppcCallerSaveRegisters64 :: [R.RegisterType PPC.PPC64]
ppcCallerSaveRegisters64 = map (Some.Some . D.Gprc . D.GPR)  [4..10]
  where
--    generalRegisters   = [0] ++ [3..12]
--    floatingRegisters  = [0..13]
--    conditionRegisters = [0,1,5,6,7,8]
--    vectorRegisters    = [0..19]

-- | Caller save registers for PPC32 (called "volitile" in the
-- [documentation]<https://gitlab-int.galois.com/brittle/sfe/uploads/793d984241f4c4546e0f81cdfe1643f4/elfspec_ppc.pdf>)
--
-- For now we are only accounting for general-purpose registers @r5@-@r12@.
ppcCallerSaveRegisters32 :: [R.RegisterType PPC.PPC32]
ppcCallerSaveRegisters32 =  map (Some.Some . D.Gprc . D.GPR)  [5..12]



-- | Create an instruction to clear a register (i.e., set it to zero or some
-- other distinguished neutral value).
--
-- XOR r r r
ppcClearRegister :: R.RegisterType PPC.PPC64 -> D.Instruction
ppcClearRegister r = D.Instruction D.XOR (coerceOperand r D.:< coerceOperand r D.:< coerceOperand r D.:< D.Nil)



coerceOperand :: Some.Some D.Operand -> D.Operand "Gprc"
coerceOperand (Some.Some (D.Gprc x)) = D.Gprc x
coerceOperand op                     = error $ "Tried to clear an unsupported register in PPC: " ++ show op
