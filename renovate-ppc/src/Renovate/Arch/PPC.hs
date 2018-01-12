{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
module Renovate.Arch.PPC (
  -- * Configuration
  config32,
  config64,
  -- * Architecture Selection
  MP.PPC64,
  MP.PPC32,
  -- * Functions
  isa,
  -- * Assembly and Disassembly
  assemble,
  disassemble,
  Instruction,
  TargetAddress(..),
  -- * ELF Support
  MP.tocBaseForELF,
  MP.tocEntryAddrsForElf,
  -- * Instruction Helpers
  toInst,
  fromInst,
  -- * Exceptions
  InstructionDisassemblyFailure(..)
  ) where

import           Data.Proxy ( Proxy(..) )
import qualified Data.Macaw.AbsDomain.AbsState as MA
import qualified Data.Macaw.CFG.Core as MC
import qualified Data.Macaw.Memory as MM
import           Data.Macaw.Types ( BVType )

import qualified Data.Macaw.PPC as MP

import           Renovate
import           Renovate.Arch.PPC.ISA

config32 :: (MM.MemWidth w, w ~ 32)
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
                 , rcBlockCallback = \_ -> return ()
                 , rcFunctionCallback = \_ _ -> return ()
                 , rcELFEntryPoints = MP.tocEntryAddrsForElf (Proxy @MP.PPC32)
                 , rcAnalysis = analysis
                 , rcRewriter = rewriter
                 , rcUpdateSymbolTable = False
                 -- See Note [Layout Addresses]
                 , rcCodeLayoutBase = 0x10080000
                 , rcDataLayoutBase = 0x100a0000
                 }

config64 :: (MM.MemWidth w, w ~ 64)
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
                 , rcBlockCallback = \_ -> return ()
                 , rcFunctionCallback = \_ _ -> return ()
                 , rcELFEntryPoints = MP.tocEntryAddrsForElf (Proxy @MP.PPC64)
                 , rcAnalysis = analysis
                 , rcRewriter = rewriter
                 , rcUpdateSymbolTable = False
                 -- See Note [Layout Addresses]
                 , rcCodeLayoutBase = 0x10080000
                 , rcDataLayoutBase = 0x100a0000
                 }

{- Note [Layout Addresses]

In PowerPC (at least in the v1 ABI for PowerPC 64), everything seems to start
around address 0x10000000.  We choose addresses far from there for our new code
and data.  Note that we can't go too far, as we need to be able to jump with a
single branch where we only have 24 bits of offset available.

-}
