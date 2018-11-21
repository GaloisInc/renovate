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
  fromFlexInst,
  annotateInstr,
  noAddr,
  prettyPrintWithAnnotations,
  x64Size,
  makeInstr,
  -- * Types
  X86.X86_64,
  Instruction,
  TargetAddress,
  Value,
  AssemblyFailure(..),
  DisassemblyFailure(..)
  ) where

import qualified Data.Macaw.X86 as X86
import qualified Data.Macaw.X86.Symbolic as SX86

import qualified Renovate as R
import           Renovate.Arch.X86_64.ABI
import           Renovate.Arch.X86_64.ISA
import           Renovate.Arch.X86_64.Internal ( Value, Instruction, TargetAddress, AssemblyFailure(..), DisassemblyFailure(..), toFlexInst, fromFlexInst, annotateInstr, noAddr, prettyPrintWithAnnotations, x64Size, makeInstr )

-- | The configuration for an x86_64 rewriter
--
-- The 'Instruction' type is an x86_64-specific opaque wrapper around
-- a flexdis86 instruction. The 'TargetAddress' annotation type is an
-- opaque target for describing symbolic branch targets and basic
-- block addresses.
--
-- This configuration is actually specific to Linux due to the system
-- call personality.
config :: callbacks X86.X86_64 binFmt a
       -- ^ An analysis (or analysis + rewriter) to be invoked by renovate on a
       -- binary.  It should be either 'R.AnalyzeOnly' or 'R.AnalyzeAndRewrite'.
       -> R.RenovateConfig X86.X86_64 binFmt callbacks a
config analysis = R.RenovateConfig
  { R.rcISA           = isa
  , R.rcABI           = abi
  , R.rcArchInfo      = const X86.x86_64_linux_info
  , R.rcAssembler     = assemble
  , R.rcDisassembler  = disassemble
  , R.rcBlockCallback = Nothing
  , R.rcFunctionCallback = Nothing
  , R.rcAnalysis      = analysis
  , R.rcUpdateSymbolTable = True
  , R.rcMaxUnconditionalJumpSize = 2^(31 :: Int)-1
  -- See Note [Layout Addresses]
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
