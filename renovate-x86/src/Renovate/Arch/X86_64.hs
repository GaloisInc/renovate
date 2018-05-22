{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
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

import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.X86 as X86
import qualified Data.Macaw.X86.Symbolic as SX86

import qualified Renovate as R
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
config :: (R.RewriteEnv X86.X86_64 -> MBL.LoadedBinary X86.X86_64 binFmt -> R.BlockInfo X86.X86_64 -> a X86.X86_64)
       -- ^ An analysis that produces a summary result to be passed into the rewriter
       -> (a X86.X86_64 -> R.ISA X86.X86_64 -> MBL.LoadedBinary X86.X86_64 binFmt -> R.SymbolicBlock X86.X86_64
             -> R.RewriteM X86.X86_64 (Maybe [R.TaggedInstruction X86.X86_64 TargetAddress]))
       -- ^ A rewriting action
       -> R.RenovateConfig X86.X86_64 binFmt a
config analysis rewriter =
  R.RenovateConfig
    { R.rcISA           = isa
    , R.rcArchInfo      = const X86.x86_64_linux_info
    , R.rcAssembler     = assemble
    , R.rcDisassembler  = disassemble
    , R.rcBlockCallback = Nothing
    , R.rcFunctionCallback = Nothing
    , R.rcELFEntryPoints = const []
    , R.rcAnalysis      = analysis
    , R.rcRewriter      = rewriter
    , R.rcUpdateSymbolTable = True
    -- See Note [Layout Addresses]
    , R.rcCodeLayoutBase = 0x800000
    , R.rcDataLayoutBase = 0xa00000
    }

instance R.ArchInfo X86.X86_64 where
  archVals _ = Just (R.ArchVals { R.archFunctions = SX86.x86_64MacawSymbolicFns
                                , R.withArchEval = \sym k -> do
                                    sfns <- SX86.newSymFuns sym
                                    k (SX86.x86_64MacawEvalFn sfns)
                                , R.withArchConstraints = \x -> x
                                })

{- Note [Layout Addresses]

The addresses chosen are somewhat arbitrary, but we choose them to try to be far
from the normal addresses in a typical binary.

In a non-position independent executable (-fno-PIE), the .text section starts at
0x400000, while the data starts at 0x600000.

In a position indepenent executable, the .text section starts at approximately
0xe00 (i.e., near 0x0).  Either way, our chosen values are pretty far and should
not overlap.

-}
