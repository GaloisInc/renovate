{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | An 'ABI' implementation for the x86_64 ABI
--
-- Currently, this is for Linux; Windows and OS X would require different ABIs
module Renovate.Arch.X86_64.ABI
( abi
, instrOperands
, instrOpcode
) where

import           Data.Bits
import           Data.Word ( Word32, Word64 )

import qualified Data.Macaw.X86 as X86

import qualified Renovate as R
import           Renovate.Arch.X86_64.Internal

import qualified Flexdis86 as D

-- | An ABI for x86_64 using flexdis.
--
-- The implementation is currently very simple.  It assumes that any
-- 'ret' instruction returns from a function that obeys the ABI.  This
-- is not true in general and needs to be made more robust for real
-- programs.
abi :: R.ABI X86.X86_64
abi = R.ABI { R.isReturn = x64IsReturn
            , R.callerSaveRegisters = x64CallerSaveRegisters
            , R.clearRegister = x64ClearRegister
            , R.allocateMemory = x64AllocateMemory
            , R.pointerSize = 8
            , R.computeStackPointerOffset = x64ComputeStackPointerOffset
            , R.saveReturnAddress = x64SaveReturnAddress
            , R.checkShadowStack = x64CheckShadowStack
            }

x64IsReturn :: forall a (tp :: R.InstructionArchReprKind X86.X86_64) . Instruction tp a -> Bool
x64IsReturn ii = instrOpcode ii == "ret"

-- | The caller-save registers that we want to clear.
--
-- According to http://agner.org/optimize/calling_conventions.pdf,
-- Chapter 6, Table 4, on Linx, BSD, and OS X on X86_64, the calling
-- convention is to return parameters in @rax@, @rdx@, @st0@, @xmm0@,
-- @ymm0@, and @zmm0@. So, we avoid clearing @rax@ and @rdx@ here.
--
-- The calling_conventions.pdf above cites
-- http://www.x86-64.org/documentation_folder/abi-0.99.pdf as its
-- primary source for X86_64 calling conventions -- that document says
-- that @st1@ is also a return register, used for returning the
-- imaginary part of complex numbers.
x64CallerSaveRegisters :: R.InstructionArchRepr X86.X86_64 tp -> [Value tp]
x64CallerSaveRegisters repr =
  case repr of
    OnlyRepr X86Repr ->
      fmap Value [ D.QWordReg D.RCX
                 , D.QWordReg D.RSI
                 , D.QWordReg D.RDI
                 , D.QWordReg (D.Reg64 8)
                 , D.QWordReg (D.Reg64 9)
                 , D.QWordReg (D.Reg64 10)
                 , D.QWordReg (D.Reg64 11)
                 ]

x64ClearRegister :: R.InstructionArchRepr X86.X86_64 tp
                 -> Value tp
                 -> Instruction tp TargetAddress
x64ClearRegister _ r =
  fmap (const NoAddress) (makeInstr "xor" [toFlexValue r, toFlexValue r])

-- | Allocate memory using mmap with an anonymous mapping
--
-- The syscall number of mmap is 9 (passed in %rax)
--
-- Arguments are passed in %rdi, %rsi, %rdx, %r10, %r8, %r9 (mmap uses all of them)
--
-- The return value is in %rax
x64AllocateMemory :: R.InstructionArchRepr X86.X86_64 tp
                  -> Word32
                  -> R.ConcreteAddress X86.X86_64
                  -> [Instruction tp TargetAddress]
x64AllocateMemory _ nBytes addr = [ noAddr $ makeInstr "push" [D.QWordReg D.RAX]
                                , noAddr $ makeInstr "push" [D.QWordReg D.RDI]
                                , noAddr $ makeInstr "push" [D.QWordReg D.RSI]
                                , noAddr $ makeInstr "push" [D.QWordReg D.RDX]
                                , noAddr $ makeInstr "push" [D.QWordReg (D.Reg64 10)]
                                , noAddr $ makeInstr "push" [D.QWordReg (D.Reg64 9)]
                                , noAddr $ makeInstr "push" [D.QWordReg (D.Reg64 8)]
                                -- Set up syscall
                                , noAddr $ makeInstr "mov" [D.QWordReg D.RAX, D.QWordImm (D.UImm64Concrete 9)] -- Syscall number
                                , noAddr $ makeInstr "xor" [D.QWordReg D.RDI, D.QWordReg D.RDI] -- Start address (no request, kernel decides)
                                , noAddr $ makeInstr "mov" [D.QWordReg D.RSI, D.QWordImm (D.UImm64Concrete (fromIntegral nBytes))] -- Amount to map
                                , noAddr $ makeInstr "mov" [D.QWordReg D.RDX, D.QWordImm (D.UImm64Concrete (prot_Read .|. prot_Write))] -- Memory protection
                                , noAddr $ makeInstr "mov" [D.QWordReg (D.Reg64 10), D.QWordImm (D.UImm64Concrete map_Anon)] -- An anonymous mapping
                                , noAddr $ makeInstr "mov" [D.QWordReg (D.Reg64 8), D.QWordImm (D.UImm64Concrete maxBound)] -- FD (should be -1)
                                , noAddr $ makeInstr "xor" [D.QWordReg (D.Reg64 9), D.QWordReg (D.Reg64 9)]
                                , noAddr $ makeInstr "syscall" []
                                -- Save the result FIXME: Error checking
                                  -- Put the address to store the result at into a register
                                , noAddr $ makeInstr "mov" [D.QWordReg D.RDI, D.QWordImm (D.UImm64Concrete (fromIntegral (R.absoluteAddress addr)))]
                                , noAddr $ makeInstr "mov" [D.Mem64 destAddr, D.QWordReg D.RAX]
                                -- Restore registers
                                , noAddr $ makeInstr "pop" [D.QWordReg (D.Reg64 8)]
                                , noAddr $ makeInstr "pop" [D.QWordReg (D.Reg64 9)]
                                , noAddr $ makeInstr "pop" [D.QWordReg (D.Reg64 10)]
                                , noAddr $ makeInstr "pop" [D.QWordReg D.RDX]
                                , noAddr $ makeInstr "pop" [D.QWordReg D.RSI]
                                , noAddr $ makeInstr "pop" [D.QWordReg D.RDI]
                                , noAddr $ makeInstr "pop" [D.QWordReg D.RAX]
                                ]
  where
    destAddr = D.Addr_64 D.DS (Just D.RDI) Nothing D.NoDisplacement

prot_Read :: Word64
prot_Read = 0x1

prot_Write :: Word64
prot_Write = 0x2

map_Anon :: Word64
map_Anon = 0x20

-- | Using RBX here because mov %rax has a strange encoding that isn't
-- supported in the assembler yet.
x64ComputeStackPointerOffset :: R.InstructionArchRepr X86.X86_64 tp
                             -> R.ConcreteAddress X86.X86_64
                             -> [Instruction tp TargetAddress]
x64ComputeStackPointerOffset _ memAddr =
  [ noAddr $ makeInstr "push" [D.QWordReg D.RBX]
  , noAddr $ makeInstr "mov" [D.QWordReg D.RBX, D.QWordImm (D.UImm64Concrete (fromIntegral (R.absoluteAddress memAddr)))]
  , noAddr $ makeInstr "sub" [D.Mem64 memRef, D.QWordReg D.RSP]
  , noAddr $ makeInstr "pop" [D.QWordReg D.RBX]
  ]
  where
    memRef = D.Addr_64 D.DS (Just D.RBX) Nothing D.NoDisplacement

-- | Since we are inserting this code at the very beginning of the
-- function body, we can clobber caller-save registers freely.  We'll
-- use rdi.
--
-- Note that this would be easier with frame pointers, but we probably
-- don't actually need them as long as we put this code before any
-- other code in the function body (esp. before the function reserves
-- stack space by modifying rsp).
x64SaveReturnAddress :: R.InstructionArchRepr X86.X86_64 tp
                     -> R.ConcreteAddress X86.X86_64
                     -> [Instruction tp TargetAddress]
x64SaveReturnAddress _ memAddr =
  [ noAddr $ makeInstr "mov" [D.QWordReg D.RDI, D.QWordImm (D.UImm64Concrete (fromIntegral (R.absoluteAddress memAddr)))]
  , noAddr $ makeInstr "mov" [D.QWordReg D.RSI, D.QWordReg D.RSP]
  , noAddr $ makeInstr "add" [D.QWordReg D.RSI, D.Mem64 offsetMemRef]
  -- The offset to write the return address to is now in %rsi (%rdi is now free)
  --
  -- Next, put the return address in %rdi, then move % to [rsi]
  , noAddr $ makeInstr "mov" [D.QWordReg D.RDI, D.Mem64 retAddrRef]
  , noAddr $ makeInstr "mov" [D.Mem64 shadowMemRef, D.QWordReg D.RDI]
  ]
  where
    offsetMemRef = D.Addr_64 D.DS (Just D.RDI) Nothing D.NoDisplacement
    shadowMemRef = D.Addr_64 D.DS (Just D.RSI) Nothing D.NoDisplacement
    -- The return address is 8(%rsp)
    retAddrRef = D.Addr_64 D.DS (Just D.RSP) Nothing (D.Disp8 8)

-- | Read the real return value into a register, read its offset
-- shadow value into another register, then cmp + jmp
x64CheckShadowStack :: forall tp
                     . R.InstructionArchRepr X86.X86_64 tp
                    -> R.ConcreteAddress X86.X86_64
                    -> [Instruction tp TargetAddress]
x64CheckShadowStack _ memAddr =
  [ noAddr $ makeInstr "mov" [D.QWordReg D.RDI, D.QWordImm (D.UImm64Concrete (fromIntegral (R.absoluteAddress memAddr)))]
  , noAddr $ makeInstr "mov" [D.QWordReg D.RSI, D.QWordReg D.RSP]
  , noAddr $ makeInstr "add" [D.QWordReg D.RSI, D.Mem64 offsetMemRef]
  -- The address of the shadow return value is now in %rsi, while %rdi is free
  , noAddr $ makeInstr "mov" [D.QWordReg D.RSI, D.Mem64 shadowMemRef]
  -- The shadow return address is now in rsi
  , noAddr $ makeInstr "mov" [D.QWordReg D.RDI, D.Mem64 retAddrRef]
  -- The real return address is now in rdi
  , noAddr $ makeInstr "cmp" [D.QWordReg D.RDI, D.QWordReg D.RSI]
  , noAddr $ makeInstr "je" [jmpOff]
  , noAddr trap
  ]
  where
    offsetMemRef = D.Addr_64 D.DS (Just D.RDI) Nothing D.NoDisplacement
    shadowMemRef = D.Addr_64 D.DS (Just D.RSI) Nothing D.NoDisplacement
    -- The return address is 8(%rsp)
    retAddrRef = D.Addr_64 D.DS (Just D.RSP) Nothing (D.Disp8 8)

    trap :: Instruction tp ()
    trap = makeInstr "int3" []
    jmpOff = D.JumpOffset D.JSize32 (D.FixedOffset (fromIntegral (x64Size trap)))
