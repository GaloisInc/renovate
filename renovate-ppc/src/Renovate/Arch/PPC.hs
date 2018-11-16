{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Renovate.Arch.PPC (
  -- * Configuration
  config32,
  config64,
  -- * Architecture Selection
  MP.PPC64,
  MP.PPC32,
  -- * Functions
  isa,
  abi32,
  abi64,
  -- * Assembly and Disassembly
  assemble,
  disassemble,
  Instruction,
  TargetAddress(..),

  -- * Exceptions
  InstructionDisassemblyFailure(..)
  ) where

import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG.Core as MC
import qualified Data.Macaw.Memory as MM

import qualified Data.Macaw.PPC as MP
import qualified Data.Macaw.BinaryLoader.PPC as BLP
-- FIXME: We probably shouldn't need this import, since the PPCReg type is
-- re-exported from Data.Macaw.PPC
import           Data.Macaw.PPC.PPCReg ()
import qualified Data.Macaw.PPC.Symbolic as MPS

import qualified Renovate as R
import           Renovate.Arch.PPC.ISA
import           Renovate.Arch.PPC.ABI

-- | A renovate configuration for 32 bit PowerPC
config32 :: ( MM.MemWidth w
            , w ~ 32
            , MC.ArchAddrWidth MP.PPC32 ~ w
            , MBL.BinaryLoader MP.PPC32 binFmt
            , BLP.HasTOC MP.PPC32 binFmt
            )
         => R.Analyze MP.PPC32 binFmt a
         -- ^ An analysis function that produces a summary result that will be
         -- fed into the rewriter
         -> R.Rewrite MP.PPC32 binFmt a
         -- ^ A rewriting action
         -> R.RenovateConfig MP.PPC32 binFmt a
config32 analysis rewriter = R.RenovateConfig
  { R.rcISA = isa
  , R.rcABI = abi32
  , R.rcArchInfo = MP.ppc32_linux_info
  , R.rcAssembler = assemble
  , R.rcDisassembler = disassemble
  , R.rcBlockCallback = Nothing
  , R.rcFunctionCallback = Nothing
  , R.rcAnalysis = analysis
  , R.rcRewriter = rewriter
  , R.rcUpdateSymbolTable = False
  -- See Note [Jump Size]
  , R.rcMaxUnconditionalJumpSize = 2^25-1
  -- See Note [Layout Addresses]
  , R.rcDataLayoutBase = 0x20000000
  }

-- | A renovate configuration for 64 bit PowerPC
config64 :: ( MM.MemWidth w
            , w ~ 64
            , MC.ArchAddrWidth MP.PPC64 ~ w
            , MBL.BinaryLoader MP.PPC64 binFmt
            , BLP.HasTOC MP.PPC64 binFmt
            )
         => R.Analyze MP.PPC64 binFmt a
         -- ^ An analysis function that produces a summary result that will be
         -- fed into the rewriter
         -> R.Rewrite MP.PPC64 binFmt a
         -- ^ A rewriting action
         -> R.RenovateConfig MP.PPC64 binFmt a
config64 analysis rewriter = R.RenovateConfig
  { R.rcISA = isa
  , R.rcABI = abi64
  , R.rcArchInfo = MP.ppc64_linux_info
  , R.rcAssembler = assemble
  , R.rcDisassembler = disassemble
  , R.rcBlockCallback = Nothing
  , R.rcFunctionCallback = Nothing
  , R.rcAnalysis = analysis
  , R.rcRewriter = rewriter
  , R.rcUpdateSymbolTable = False
  -- See Note [Jump Size]
  , R.rcMaxUnconditionalJumpSize = 2^25-1
  -- See Note [Layout Addresses]
  , R.rcDataLayoutBase = 0x20000000
  }

instance R.ArchInfo MP.PPC64 where
  archVals _ = Just (R.ArchVals { R.archFunctions = MPS.ppc64MacawSymbolicFns
                                , R.withArchEval = \sym k -> do
                                    sfns <- MPS.newSymFuns sym
                                    k (MPS.ppc64MacawEvalFn sfns)
                                , R.withArchConstraints = \x -> x
                                })

instance R.ArchInfo MP.PPC32 where
  archVals _ = Just (R.ArchVals { R.archFunctions = MPS.ppc32MacawSymbolicFns
                                , R.withArchEval = \sym k -> do
                                    sfns <- MPS.newSymFuns sym
                                    k (MPS.ppc32MacawEvalFn sfns)
                                , R.withArchConstraints = \x -> x
                                })


{- Note [Layout Addresses]

In PowerPC (at least in the v1 ABI for PowerPC 64), everything seems to start
around address 0x10000000.  We choose addresses far from there for our new code
and data.  Note that we can't go too far, as we need to be able to jump with a
single branch where we only have 24 bits of offset available.

-}
{- Note [Jump Size]
There are 24 bits of offset in the branch instruction, plus 2 zero bits are
added for you at the end -- but it's signed, so lose one bit for that for a
total of 25 bits in either direction.
-}
