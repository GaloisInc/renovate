{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Lift concrete blocks into relocatable symbolic blocks
module Renovate.Redirect.Symbolize (
  SymbolicAddressAllocator,
  symbolicAddressAllocator,
  nextSymbolicAddress,
  symbolizeBasicBlocks
  ) where

import qualified Data.Foldable as F
import qualified Data.Macaw.CFG as MM
import qualified Data.Map as M
import           Data.Maybe ( fromMaybe )
import qualified Data.Traversable as T
import           Data.Word ( Word64 )

import           Prelude

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA

newtype SymbolicAddressAllocator arch = SymbolicAddressAllocator Word64

symbolicAddressAllocator :: SymbolicAddressAllocator arch
symbolicAddressAllocator = SymbolicAddressAllocator 0

nextSymbolicAddress :: SymbolicAddressAllocator arch -> (SymbolicAddress arch, SymbolicAddressAllocator arch)
nextSymbolicAddress (SymbolicAddressAllocator i) = (SymbolicAddress i, SymbolicAddressAllocator (i + 1))

-- | Convert concrete blocks into symbolic blocks.
--
-- This requires:
--
-- 1) assigning symbolic addresses to each basic block, and
--
-- 2) updating all of the (direct) jump instructions with their
--    symbolic targets.
--
-- Indirect jumps do not need to be annotated (in part because we
-- cannot annotate them).
symbolizeBasicBlocks :: (T.Traversable t, InstructionConstraints arch)
                     => ISA arch
                     -> MM.Memory (MM.ArchAddrWidth arch)
                     -> SymbolicAddressAllocator arch
                     -> t (ConcreteBlock arch)
                     -> (SymbolicAddressAllocator arch, t (ConcreteBlock arch, SymbolicBlock arch))
symbolizeBasicBlocks isa mem symAlloc0 concreteBlocks =
  (symAlloc1, fmap (symbolizeJumps isa mem blockAddressIndex) symBlocks)
  where
    (symAlloc1, symBlocks) = T.mapAccumL allocateSymbolicAddress symAlloc0 concreteBlocks
    blockAddressIndex = M.fromList [ (basicBlockAddress b, symAddr)
                                   | (b, symAddr) <- F.toList symBlocks
                                   ]

    allocateSymbolicAddress symAlloc cb =
      let (addr, symAlloc') = nextSymbolicAddress symAlloc
      in (symAlloc', (cb, addr))

-- | For the given 'SymbolicBlock', traverse all of its instructions
-- and tag them with their symbolic destination, if any.
--
-- We can find the symbolic destination based from the map of absolute
-- addresses to symbolic blocks.
--
-- For relative jumps, we need to compute the address of the
-- instruction and then add the offset of the relative jump (and look
-- the result up in that same map).
--
-- See Note [Jump Promotion]
symbolizeJumps :: (InstructionConstraints arch)
               => ISA arch
               -> MM.Memory (MM.ArchAddrWidth arch)
               -> M.Map (ConcreteAddress arch) (SymbolicAddress arch)
               -> (ConcreteBlock arch, SymbolicAddress arch)
               -> (ConcreteBlock arch, SymbolicBlock arch)
symbolizeJumps isa mem symAddrMap (cb, symAddr) =
  (cb, BasicBlock { basicBlockAddress = SymbolicInfo { symbolicAddress = symAddr
                                                     , concreteAddress = basicBlockAddress cb
                                                     }
                  , basicBlockInstructions = concat insns
                  })
  where
    lookupSymAddr ca = M.lookup ca symAddrMap
    insns = fmap symbolize (instructionAddresses isa cb)

    symbolize (i, addr) =
      case isaJumpType isa i mem addr of
        AbsoluteJump _ target ->
          let symTarget = lookupSymbolicAddress target
          in isaSymbolizeAddresses isa mem lookupSymAddr addr (Just symTarget) i
        RelativeJump _ _ offset ->
          let symTarget = lookupSymbolicAddress (addr `addressAddOffset` offset)
          in isaSymbolizeAddresses isa mem lookupSymAddr addr (Just symTarget) i
        IndirectJump _ ->
          -- We do not know the destination of indirect jumps, so we
          -- can't tag them (or rewrite them later)
          isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
        DirectCall _ offset ->
          let symTarget = lookupSymbolicAddress (addr `addressAddOffset` offset)
          in isaSymbolizeAddresses isa mem lookupSymAddr addr (Just symTarget) i
        IndirectCall -> isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
        Return _ -> isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
        NoJump -> isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i

    lookupSymbolicAddress target = fromMaybe (StableAddress target) (M.lookup target symAddrMap)

{- Note [Jump Promotion]

While we are making addresses symbolic, we also have to promote short jump
instructions to full length jumps in case we need to jump farther during code
relocation.  We use 'isaModifyJumpTarget' for that purpose, as it chooses the
biggest jump that it can.  Unfortunately, due to the types, we have to do that
*before* we call 'isaSymbolizeAddresses', which changes the operand annotation.
The actual address used for isaModifyJumpTarget isn't important, as we'll tag on
the correct symbolic address immediately.

This happens to be fine for now, as 'isaSymbolizeAddresses' only deals with
operands that deal with memory addresses, while 'isaModifyJumpTarget' only deals
with operands of jump instructions.

This probably only affects x86.

-}
