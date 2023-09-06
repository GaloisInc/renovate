{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ISA.T32
  ) where

import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.ARM as MA32
import           Data.Macaw.AArch32.Symbolic ()
import           Data.Macaw.BinaryLoader.AArch32 ()

import qualified Renovate as R
import qualified Renovate.Arch.AArch32.ISA as ISA
import qualified Renovate.Arch.AArch32.ABI as ABI

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
  }
