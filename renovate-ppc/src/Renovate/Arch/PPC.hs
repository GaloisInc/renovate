{-# LANGUAGE DataKinds #-}
module Renovate.Arch.PPC (
  -- * Configuration
  config32,
  config64,
  -- * Functions
  isa,
  -- * Assembly and Disassembly
  assemble,
  disassemble,
  Instruction,
  TargetAddress(..),
  -- * Helpers
  toInst,
  fromInst,
  -- * Exceptions
  InstructionDisassemblyFailure(..)
  ) where

import qualified Data.Macaw.AbsDomain.AbsState as MA
import qualified Data.Macaw.CFG.Core as MC
import qualified Data.Macaw.Memory as MM
import           Data.Macaw.Types ( BVType )

import qualified Data.Macaw.PPC as MP

import           Renovate
import           Renovate.Arch.PPC.ISA

config32 :: (MM.MemWidth w)
         => (MC.ArchSegmentOff MP.PPC32 -> Maybe (MA.AbsValue 32 (BVType 32)))
         -> (ISA Instruction (TargetAddress w) w -> MM.Memory w -> BlockInfo Instruction w MP.PPC32 -> a)
         -> (a -> ISA Instruction (TargetAddress w) w -> MM.Memory w -> SymbolicBlock Instruction (TargetAddress w) w
               -> RewriteM Instruction w (Maybe [TaggedInstruction Instruction (TargetAddress w)]))
         -> RenovateConfig Instruction (TargetAddress w) w MP.PPC32 a
config32 tocMap analysis rewriter =
  RenovateConfig { rcISA = isa
                 , rcArchInfo = MP.ppc32_linux_info tocMap
                 , rcAssembler = assemble
                 , rcDisassembler = disassemble
                 , rcAnalysis = analysis
                 , rcRewriter = rewriter
                 }

config64 :: (MM.MemWidth w)
         => (MC.ArchSegmentOff MP.PPC64 -> Maybe (MA.AbsValue 64 (BVType 64)))
         -> (ISA Instruction (TargetAddress w) w -> MM.Memory w -> BlockInfo Instruction w MP.PPC64 -> a)
         -> (a -> ISA Instruction (TargetAddress w) w -> MM.Memory w -> SymbolicBlock Instruction (TargetAddress w) w
               -> RewriteM Instruction w (Maybe [TaggedInstruction Instruction (TargetAddress w)]))
         -> RenovateConfig Instruction (TargetAddress w) w MP.PPC64 a
config64 tocMap analysis rewriter =
  RenovateConfig { rcISA = isa
                 , rcArchInfo = MP.ppc64_linux_info tocMap
                 , rcAssembler = assemble
                 , rcDisassembler = disassemble
                 , rcAnalysis = analysis
                 , rcRewriter = rewriter
                 }
