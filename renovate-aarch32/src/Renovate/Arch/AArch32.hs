{-# LANGUAGE FlexibleContexts #-}
module Renovate.Arch.AArch32 (
  config,
  ISA.AArch32,
  ISA.isa,
  ISA.assemble,
  ISA.disassemble,
  ISA.Instruction,
  ISA.TargetAddress(..),
  ISA.InstructionDisassemblyFailure(..),
  ISA.ARMRepr(..),
  R.InstructionArchRepr(ISA.ArchRepr),
  ISA.A32,
  ISA.T32
  ) where

import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG.Core as MC
import qualified Data.Macaw.Memory as MM

-- TODO: Macaw imports after Ben gets them compiling
-- import qualified Data.Macaw.AArch32 as MA32
-- import qualified Data.Macaw.BinaryLoader.AArch32 as MBLA32

import qualified Renovate as R
import qualified Renovate.Arch.AArch32.ISA as ISA
import qualified Renovate.Arch.AArch32.ABI as ABI

config :: (MBL.BinaryLoader ISA.AArch32 binFmt)
       => callbacks ISA.AArch32 binFmt a
       -> R.RenovateConfig ISA.AArch32 binFmt callbacks a
config analysis = R.RenovateConfig
  { R.rcISA = ISA.isa
  , R.rcABI = ABI.abi
  , R.rcArchInfo = error "MA32.aarch32_linux_info"
  , R.rcAssembler = ISA.assemble
  , R.rcDisassembler = ISA.disassemble
  , R.rcBlockCallback = Nothing
  , R.rcFunctionCallback = Nothing
  , R.rcAnalysis = analysis
  , R.rcUpdateSymbolTable = True
  , R.rcDataLayoutBase = 0xf000
  , R.rcExtratextOffset = 0
  , R.rcRefinementConfig = Nothing
  }
