module Renovate.Arch.AArch32 (
  config,
  AArch32,
  isa,
  assemble,
  disassemble,
  Instruction(..),
  TargetAddress(..),
  InstructionDisassemblyFailure(..)
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

config :: (MBL.BinaryLoader AArch32 binFmt)
       => callbacks AArch32 binFmt a
       -> R.RenovateConfig AArch32 binFmt callbacks a
config analysis = R.RenovateConfig
  { R.rcISA = ISA.isa
  , R.rcABI = ABI.abi
  , R.rcArchInfo = MA32.aarch32_linux_info
  , R.rcAssembler = ISA.assemble
  , R.rcDisassembler = ISA.disassemble
  , R.rcBlockCallback = Nothing
  , R.rcFunctionCallback = Nothing
  , R.rcAnalysis = analysis
  , R.rcUpdateSymbolTable = True
  , R.rcDataLayoutBase = error "layout base"
  , R.rcExtratextOffset = 0
  }
