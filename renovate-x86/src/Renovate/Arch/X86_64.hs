{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
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
import qualified Data.Macaw.X86.ArchTypes as X86

import           Renovate
import           Renovate.Arch.X86_64.ABI
import           Renovate.Arch.X86_64.ISA
import           Renovate.Arch.X86_64.Internal ( Value, Instruction, TargetAddress, AssemblyFailure(..), DisassemblyFailure(..) )

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
    , rcBlockCallback = \_ -> return ()
    , rcFunctionCallback = \_ _ -> return ()
    , rcELFEntryPoints = const []
    , rcAnalysis      = analysis
    , rcRewriter      = rewriter
    }
