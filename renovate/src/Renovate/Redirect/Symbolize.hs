{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Lift concrete blocks into relocatable symbolic blocks
module Renovate.Redirect.Symbolize (
  SymbolicAddressAllocator,
  symbolicAddressAllocator,
  nextSymbolicAddress,
  symbolizeBasicBlocks
  ) where

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Macaw.CFG as MM
import qualified Data.Map as M
import           Data.Maybe ( fromMaybe )
import qualified Data.Traversable as T
import           Data.Word ( Word64 )

import           Prelude

import           Renovate.Core.Address
import           Renovate.Core.BasicBlock
import qualified Renovate.Core.Instruction as RCI
import qualified Renovate.Core.Relocation as RCR
import           Renovate.ISA
import qualified Renovate.Panic as RP
import qualified Renovate.Redirect.ReifyFallthrough as RRR

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
symbolizeBasicBlocks :: (T.Traversable t, MM.MemWidth (MM.ArchAddrWidth arch))
                     => ISA arch
                     -> MM.Memory (MM.ArchAddrWidth arch)
                     -> SymbolicAddressAllocator arch
                     -> t (ConcreteBlock arch)
                     -> (SymbolicAddressAllocator arch, t (ConcreteBlock arch, SymbolicBlock arch))
symbolizeBasicBlocks isa mem symAlloc0 concreteBlocks =
  (symAlloc1, fmap (symbolizeJumps isa mem blockAddressIndex) symBlocks)
  where
    (symAlloc1, symBlocks) = T.mapAccumL allocateSymbolicAddress symAlloc0 concreteBlocks
    blockAddressIndex = M.fromList [ (concreteBlockAddress b, symAddr)
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
symbolizeJumps :: forall arch
                . (MM.MemWidth (MM.ArchAddrWidth arch))
               => ISA arch
               -> MM.Memory (MM.ArchAddrWidth arch)
               -> M.Map (ConcreteAddress arch) (SymbolicAddress arch)
               -> (ConcreteBlock arch, SymbolicAddress arch)
               -> (ConcreteBlock arch, SymbolicBlock arch)
symbolizeJumps isa mem symAddrMap (cb, symAddr) =
  withInstructionAddresses isa cb $ \repr insnAddrs0 ->
    let insnAddrs1 = fmap symbolize insnAddrs0
    in case DLN.nonEmpty (concat insnAddrs1) of
         Nothing ->
           RP.panic RP.Symbolize "symbolizeJumps" [ "Created empty block while symbolizing block at: " ++ show (concreteBlockAddress cb)
                                                  ]
         Just insnList ->
           case RRR.reifyFallthrough isa mem cb of
             Nothing -> (cb, symbolicBlock (concreteBlockAddress cb) symAddr insnList repr Nothing)
             Just concreteFallthrough ->
               -- NOTE: It is not a failure if there is no symbolic address for a
               -- fallthrough address.  That simply means that code discovery was
               -- unable to discover that code for some reason.  We call that
               -- fallthrough a 'StableAddress' and make sure that, wherever we move
               -- the block with this fallthrough to, it will go back to the address
               -- it originally fell through to.
               let symSucc = lookupSymbolicAddress concreteFallthrough
                   concAddr = concreteBlockAddress cb
               in (cb, symbolicBlock concAddr symAddr insnList repr (Just symSucc))
  where
    symbolize :: forall tp
               . (RCI.Instruction arch tp (), ConcreteAddress arch)
              -> [RCI.Instruction arch tp (RCR.Relocation arch)]
    symbolize (i, addr) =
      isaSymbolizeAddresses isa mem lookupSymbolicAddress addr i

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
