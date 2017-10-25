module Renovate.Arch.PPC (
  -- * Configuration
  config32,
  config64,
  -- * Functions
  isa,
  -- * Assembly and Disassembly
  assemble,
  disassemble,
  disassemble1,
  Instruction,
  TargetAddress(..),
  -- * Helpers
  toInst,
  fromInst,
  -- * Exceptions
  InstructionDisassemblyFailure(..)
  ) where

import qualified Data.Macaw.Memory as MM

import qualified SemMC.Architecture.PPC32 as PPC32
import qualified SemMC.Architecture.PPC64 as PPC64

import qualified Data.Macaw.PPC as MP

import           Renovate
import           Renovate.Arch.PPC.ISA

config32 :: (MM.MemWidth w)
         => (ISA Instruction (TargetAddress w) w -> MM.Memory w -> BlockInfo Instruction w PPC32.PPC -> a)
         -> (a -> ISA Instruction (TargetAddress w) w -> MM.Memory w -> SymbolicBlock Instruction (TargetAddress w) w
               -> RewriteM Instruction w (Maybe [TaggedInstruction Instruction (TargetAddress w)]))
         -> RenovateConfig Instruction (TargetAddress w) w PPC32.PPC a
config32 analysis rewriter =
  RenovateConfig { rcISA = isa
                 , rcArchInfo = MP.ppc32_linux_info
                 , rcAssembler = assemble
                 , rcDisassembler = disassemble
                 , rcDisassembler1 = disassemble1
                 , rcAnalysis = analysis
                 , rcRewriter = rewriter
                 }

config64 :: (MM.MemWidth w)
         => (ISA Instruction (TargetAddress w) w -> MM.Memory w -> BlockInfo Instruction w PPC64.PPC -> a)
         -> (a -> ISA Instruction (TargetAddress w) w -> MM.Memory w -> SymbolicBlock Instruction (TargetAddress w) w
               -> RewriteM Instruction w (Maybe [TaggedInstruction Instruction (TargetAddress w)]))
         -> RenovateConfig Instruction (TargetAddress w) w PPC64.PPC a
config64 analysis rewriter =
  RenovateConfig { rcISA = isa
                 , rcArchInfo = MP.ppc64_linux_info
                 , rcAssembler = assemble
                 , rcDisassembler = disassemble
                 , rcDisassembler1 = disassemble1
                 , rcAnalysis = analysis
                 , rcRewriter = rewriter
                 }
