{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Renovate.Arch.AArch32 (
  config,
  ISA.AArch32,
  ISA.isa,
  ISA.assemble,
  ISA.disassemble,
  ISA.Instruction,
  ISA.InstructionDisassemblyFailure(..),
  ISA.ARMRepr(..),
  ISA.A32,
  ISA.T32,
  ISA.Operand(..),
  ISA.armJumpType,
  pattern ISA.AI,
  ISA.noRelocations,
  ISA.ldr_i,
  ISA.add_r,
  ISA.push,
  ISA.pop,
  ISA.cmp_r,
  ISA.getCond,
  ISA.jumpOff,
  ISA.unconditional,
  ISA.exit_call,
  ISA.svc,
  ISA.mov32
  ) where

import qualified Data.Map as M
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.ARM as MA32
import           Data.Macaw.AArch32.Symbolic ()
import           Data.Macaw.BinaryLoader.AArch32 ()

import qualified Renovate as R
import qualified Renovate.Arch.AArch32.ISA as ISA
import qualified Renovate.Arch.AArch32.ABI as ABI
import Data.Maybe (catMaybes)

config :: (MBL.BinaryLoader ISA.AArch32 binFmt)
       => callbacks ISA.AArch32 binFmt a
       -> R.RenovateConfig ISA.AArch32 binFmt callbacks a
config analysis = R.RenovateConfig
  { R.rcISA = ISA.isa
  , R.rcABI = ABI.abi
  , R.rcArchInfo = const MA32.arm_linux_info
  , R.rcAssembler = ISA.assemble
  , R.rcDisassembler = ISA.disassemble
  , R.rcFunctionCallback = Nothing
  , R.rcAnalysis = analysis
  , R.rcUpdateSymbolTable = True
  , R.rcDataLayoutBase = 0xb0000
  , R.rcExtratextOffset = 0
  , R.rcRefinementConfig = Nothing
  , R.rcFunctionReturnStatus = \sam ->
      catMaybes $ map(\(addr,nm) -> if nm `elem` ["exit", "_Exit", "perror"] then Just (addr,MD.NoReturnFun) else Nothing) (M.toList sam)
  }
