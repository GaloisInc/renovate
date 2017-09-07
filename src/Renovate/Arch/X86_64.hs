{-# LANGUAGE FlexibleContexts #-}
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
  disassemble1,
  -- * Types
  Instruction,
  TargetAddress,
  Value,
  AssemblyFailure(..),
  DisassemblyFailure(..)
  ) where

import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.X86 as X86
import qualified Data.Macaw.X86.ArchTypes as X86

import           Renovate.Config.Internal ( RewriterConfig(..) )

import           Renovate.Arch.X86_64.ABI
import           Renovate.Arch.X86_64.ISA
import           Renovate.Arch.X86_64.Internal ( Value, Instruction, TargetAddress, AssemblyFailure(..), DisassemblyFailure(..) )
import           Renovate.BasicBlock

-- | The configuration for an x86_64 rewriter
--
-- The 'Instruction' type is an x86_64-specific opaque wrapper around
-- a flexdis86 instruction. The 'TargetAddress' annotation type is an
-- opaque target for describing symbolic branch targets and basic
-- block addresses.
--
-- This configuration is actually specific to Linux due to the system
-- call personality.
config :: (MM.MemWidth w) => RewriterConfig Instruction (TargetAddress w) w X86.X86_64
config = RewriterConfig { rcISA = isa
                        , rcArchInfo = X86.x86_64_linux_info
                        , rcAssembler = assemble
                        , rcDisassembler = disassemble
                        , rcDisassembler1 = disassemble1
                        , rcInstrumentor = instrumentor
                        }
  where
    instrumentor = return . basicBlockInstructions
