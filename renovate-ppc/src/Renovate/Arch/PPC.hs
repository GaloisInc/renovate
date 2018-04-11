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

import qualified Renovate as R
import           Renovate.Arch.PPC.ISA

-- | A renovate configuration for 32 bit PowerPC
config32 :: (MM.MemWidth w, w ~ 32, MC.ArchAddrWidth MP.PPC32 ~ w)
         => (MC.ArchSegmentOff MP.PPC32 -> Maybe (MA.AbsValue 32 (BVType 32)))
         -- ^ A mapping from a function entry point address to a (macaw)
         -- abstract value that represents the value in the TOC register at the
         -- entry point to that function.  The value is the address of the table
         -- of contents for the module containing the function.
         -> (R.ISA MP.PPC32 -> MM.Memory w -> R.BlockInfo MP.PPC32 -> a MP.PPC32)
         -- ^ An analysis function that produces a summary result that will be
         -- fed into the rewriter
         -> (a MP.PPC32 -> R.ISA MP.PPC32 -> MM.Memory w -> R.SymbolicBlock MP.PPC32
               -> R.RewriteM MP.PPC32 (Maybe [R.TaggedInstruction MP.PPC32 (TargetAddress MP.PPC32)]))
         -- ^ A rewriting action
         -> R.RenovateConfig MP.PPC32 a
config32 tocMap analysis rewriter =
  R.RenovateConfig { R.rcISA = isa
                   , R.rcArchInfo = MP.ppc32_linux_info tocMap
                   , R.rcAssembler = assemble
                   , R.rcDisassembler = disassemble
                   , R.rcBlockCallback = Nothing
                   , R.rcFunctionCallback = Nothing
                   , R.rcELFEntryPoints = MP.tocEntryAddrsForElf (Proxy @MP.PPC32)
                   , R.rcAnalysis = analysis
                   , R.rcRewriter = rewriter
                   , R.rcUpdateSymbolTable = False
                   -- See Note [Layout Addresses]
                   , R.rcCodeLayoutBase = 0x10100000
                   , R.rcDataLayoutBase = 0x20000000
                   }

-- | A renovate configuration for 64 bit PowerPC
config64 :: (MM.MemWidth w, w ~ 64, MC.ArchAddrWidth MP.PPC64 ~ w)
         => (MC.ArchSegmentOff MP.PPC64 -> Maybe (MA.AbsValue 64 (BVType 64)))
         -- ^ A mapping from a function entry point address to a (macaw)
         -- abstract value that represents the value in the TOC register at the
         -- entry point to that function.  The value is the address of the table
         -- of contents for the module containing the function.
         -> (R.ISA MP.PPC64 -> MM.Memory w -> R.BlockInfo MP.PPC64 -> a MP.PPC64)
         -- ^ An analysis function that produces a summary result that will be
         -- fed into the rewriter
         -> (a MP.PPC64 -> R.ISA MP.PPC64 -> MM.Memory w -> R.SymbolicBlock MP.PPC64
               -> R.RewriteM MP.PPC64 (Maybe [R.TaggedInstruction MP.PPC64 (TargetAddress MP.PPC64)]))
         -- ^ A rewriting action
         -> R.RenovateConfig MP.PPC64 a
config64 tocMap analysis rewriter =
  R.RenovateConfig { R.rcISA = isa
                   , R.rcArchInfo = MP.ppc64_linux_info tocMap
                   , R.rcAssembler = assemble
                   , R.rcDisassembler = disassemble
                   , R.rcBlockCallback = Nothing
                   , R.rcFunctionCallback = Nothing
                   , R.rcELFEntryPoints = MP.tocEntryAddrsForElf (Proxy @MP.PPC64)
                   , R.rcAnalysis = analysis
                   , R.rcRewriter = rewriter
                   , R.rcUpdateSymbolTable = False
                   -- See Note [Layout Addresses]
                   , R.rcCodeLayoutBase = 0x10100000
                   , R.rcDataLayoutBase = 0x20000000
                   }

instance R.ArchInfo MP.PPC64 where
  archFunctions _ = Nothing

instance R.ArchInfo MP.PPC32 where
  archFunctions _ = Nothing

{- Note [Layout Addresses]

In PowerPC (at least in the v1 ABI for PowerPC 64), everything seems to start
around address 0x10000000.  We choose addresses far from there for our new code
and data.  Note that we can't go too far, as we need to be able to jump with a
single branch where we only have 24 bits of offset available.

-}
