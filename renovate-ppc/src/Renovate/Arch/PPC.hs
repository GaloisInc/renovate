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

import qualified SemMC.Architecture.PPC32 as PPC32
import qualified SemMC.Architecture.PPC64 as PPC64

import qualified Data.Macaw.PPC as MP

import           Renovate
import           Renovate.Arch.PPC.ISA

config32 :: (MM.MemWidth w)
         => (MC.ArchSegmentOff PPC32.PPC -> Maybe (MA.AbsValue 32 (BVType 32)))
         -> (ISA Instruction (TargetAddress w) w -> MM.Memory w -> BlockInfo Instruction w PPC32.PPC -> a)
         -> (a -> ISA Instruction (TargetAddress w) w -> MM.Memory w -> SymbolicBlock Instruction (TargetAddress w) w
               -> RewriteM Instruction w (Maybe [TaggedInstruction Instruction (TargetAddress w)]))
         -> RenovateConfig Instruction (TargetAddress w) w PPC32.PPC a
config32 tocMap analysis rewriter =
  RenovateConfig { rcISA = isa
                 , rcArchInfo = MP.ppc32_linux_info tocMap
                 , rcAssembler = assemble
                 , rcDisassembler = disassemble
                 , rcAnalysis = analysis
                 , rcRewriter = rewriter
                 }

config64 :: (MM.MemWidth w)
         => (MC.ArchSegmentOff PPC64.PPC -> Maybe (MA.AbsValue 64 (BVType 64)))
         -> (ISA Instruction (TargetAddress w) w -> MM.Memory w -> BlockInfo Instruction w PPC64.PPC -> a)
         -> (a -> ISA Instruction (TargetAddress w) w -> MM.Memory w -> SymbolicBlock Instruction (TargetAddress w) w
               -> RewriteM Instruction w (Maybe [TaggedInstruction Instruction (TargetAddress w)]))
         -> RenovateConfig Instruction (TargetAddress w) w PPC64.PPC a
config64 tocMap analysis rewriter =
  RenovateConfig { rcISA = isa
                 , rcArchInfo = MP.ppc64_linux_info tocMap
                 , rcAssembler = assemble
                 , rcDisassembler = disassemble
                 , rcAnalysis = analysis
                 , rcRewriter = rewriter
                 }
