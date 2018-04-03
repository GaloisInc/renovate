{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | The interface for X86_64-specific ISA details.
--
-- The x86_64 ISA is currently supported through the flexdis86
-- library.  It is the intent that no references to flexdis86 appear
-- outside of this module (and its related submodules).
module Renovate.Arch.X86_64 (
  -- * Configuration
  config,
  -- * Functions
  isa,
  abi,
  -- * Assembly
  assemble,
  disassemble,
  -- * Utilities
  instrOpcode,
  instrOperands,
  toFlexInst,
  -- * Types
  X86.X86_64,
  Instruction,
  TargetAddress,
  Value,
  AssemblyFailure(..),
  DisassemblyFailure(..)
  ) where

import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.X86 as X86
import qualified Data.Macaw.X86.Symbolic as SX86

import           Renovate
import           Renovate.Arch.X86_64.ABI
import           Renovate.Arch.X86_64.ISA
import           Renovate.Arch.X86_64.Internal ( Value, Instruction, TargetAddress, AssemblyFailure(..), DisassemblyFailure(..), toFlexInst )

-- | The configuration for an x86_64 rewriter
--
-- The 'Instruction' type is an x86_64-specific opaque wrapper around
-- a flexdis86 instruction. The 'TargetAddress' annotation type is an
-- opaque target for describing symbolic branch targets and basic
-- block addresses.
--
-- This configuration is actually specific to Linux due to the system
-- call personality.
config :: (MM.MemWidth w)
       => (ISA Instruction (TargetAddress w) w -> MM.Memory w -> BlockInfo Instruction w X86.X86_64 -> a)
       -> (a -> ISA Instruction (TargetAddress w) w -> MM.Memory w -> SymbolicBlock Instruction (TargetAddress w) w
             -> RewriteM Instruction w (Maybe [TaggedInstruction Instruction (TargetAddress w)]))
       -> RenovateConfig Instruction (TargetAddress w) w X86.X86_64 a
config analysis rewriter =
  RenovateConfig
    { rcISA           = isa
    , rcArchInfo      = X86.x86_64_linux_info
    , rcAssembler     = assemble
    , rcDisassembler  = disassemble
    , rcBlockCallback = Nothing
    , rcFunctionCallback = Nothing
    , rcELFEntryPoints = const []
    , rcAnalysis      = analysis
    , rcRewriter      = rewriter
    , rcUpdateSymbolTable = True
    -- See Note [Layout Addresses]
    , rcCodeLayoutBase = 0x800000
    , rcDataLayoutBase = 0xa00000
    }

instance ArchInfo X86.X86_64 where
  archFunctions _ = Just SX86.x86_64MacawSymbolicFns

{- Note [Layout Addresses]

The addresses chosen are somewhat arbitrary, but we choose them to try to be far
from the normal addresses in a typical binary.

In a non-position independent executable (-fno-PIE), the .text section starts at
0x400000, while the data starts at 0x600000.

In a position indepenent executable, the .text section starts at approximately
0xe00 (i.e., near 0x0).  Either way, our chosen values are pretty far and should
not overlap.

-}
