{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Data.Macaw.Discovery as MC
import qualified Data.Map as M
import           Data.Maybe ( fromMaybe, mapMaybe, listToMaybe )
import qualified Data.Traversable as T
import           Data.Word ( Word64 )
import qualified Data.Vector as Vec
import           Data.Parameterized.Some


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
symbolizeJumps :: (InstructionConstraints arch)
               => ISA arch
               -> MM.Memory (MM.ArchAddrWidth arch)
               -> M.Map (ConcreteAddress arch) (SymbolicAddress arch)
               -> (ConcreteBlock arch, SymbolicAddress arch)
               -> (ConcreteBlock arch, SymbolicBlock arch)
symbolizeJumps isa mem symAddrMap (cb, symAddr) =
  case DLN.nonEmpty (concat insns) of
    Nothing -> error ("Created empty block while symbolizing block at: " ++ show (concreteBlockAddress cb))
    Just insnList ->
      (cb, symbolicBlock (concreteBlockAddress cb) symAddr insnList)
  where
    lookupSymAddr ca = M.lookup ca symAddrMap
    insns = fmap symbolize (instructionAddresses isa cb)
    pblock = concreteDiscoveryBlock cb

    symbolize (i, addr) =
      case isaJumpType isa i mem addr of
        AbsoluteJump _ target ->
          let symTarget = lookupSymbolicAddress target
          in isaSymbolizeAddresses isa mem lookupSymAddr addr (Just symTarget) i
        RelativeJump _ _ offset ->
          let symTarget = lookupSymbolicAddress (addr `addressAddOffset` offset)
          in isaSymbolizeAddresses isa mem lookupSymAddr addr (Just symTarget) i
        IndirectJump _ ->
          case getParsedJumpTableTarget symAddrMap addr pblock of
            -- If the indirect jump is known to be a lookup
            -- into a jump table, rewrite according ot that.
            Just tableInfo ->
              maybe
              (error $
                "isaSymbolizeLookupJump failed to "
                ++ "symbolize lookup jump at "++show addr)
              id
              (isaSymbolizeLookupJump isa tableInfo)
          -- Otherwise we do not know the destination of
          -- indirect jumps, so we can't tag them (or
          -- rewrite them later)
            Nothing -> isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
        DirectCall _ offset ->
          let symTarget = lookupSymbolicAddress (addr `addressAddOffset` offset)
          in isaSymbolizeAddresses isa mem lookupSymAddr addr (Just symTarget) i
        IndirectCall -> isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
        Return _ -> isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i
        NoJump -> isaSymbolizeAddresses isa mem lookupSymAddr addr Nothing i

    lookupSymbolicAddress target = fromMaybe (StableAddress target) (M.lookup target symAddrMap)


-- | Given a ConcreteAddress and a ParsedBlock from Macaw,
-- check if the given address is both the final instruction
-- in the block and corresponds to a ParsedLookupTable that
-- Macaw discovered, and if so return the reachable
-- addresses.
getParsedJumpTableTarget :: forall arch .
  (InstructionConstraints arch, MM.MemWidth (MM.ArchAddrWidth arch))
  => M.Map (ConcreteAddress arch) (SymbolicAddress arch)
  -> ConcreteAddress arch
  -> Some (MC.ParsedBlock arch)
  -> Maybe (SymbolicLookupTableInfo arch)
getParsedJumpTableTarget symAddrMap caddr (Some b) =
  case (maybeLastStmtOffset, MC.pblockTermStmt b) of
    (Nothing, _) ->
      error $
      "Renovate could not locate the last instruction"
      ++ " in the Macaw ParsedBlock associated with address "
      ++show caddr
    ( Just offset
      , (MC.ParsedLookupTable
         regState
         idxAddr
         tgtAddrs))
     | Just caddr == concretizeMacawStmtAddr addr0 offset ->
       let tgtCAddrs = Vec.fromList
                       $ mapMaybe concretizeMacawAddr
                       $ Vec.toList tgtAddrs
       in if Vec.length tgtCAddrs == Vec.length tgtAddrs
          then Just $ SymbolicLookupTableInfo
               { symbolicLookupRegs = regState
               , symbolicLookupIdx = idxAddr
               , symbolicLookupAddrs = fmap lookupSymAddr tgtCAddrs
               }
          else error $
               "Renovate could not calculate concrete addresses for all of the "
               ++show (Vec.length tgtAddrs)
               ++" jump table targets associated with "
               ++show caddr
               ++"\n Macaw Segment Offsets: "
               ++show (Vec.toList tgtAddrs)
               ++"\n Converted concrete addresses: "
               ++show (map (concretizeMacawAddr @arch) $ Vec.toList tgtAddrs)
    
    (_,_) -> Nothing
  where -- | Block starting address
        addr0 = MC.pblockAddr b
        -- | The last statement offset (if one was found)
        maybeLastStmtOffset = listToMaybe
                              $ mapMaybe (\case
                                             MM.InstructionStart a _ -> Just a
                                             _ -> Nothing)
                              $ reverse $ MC.pblockStmts b
          -- | Convert a ConcreteAddress to a SymbolicAddress (if possible)
        lookupSymAddr tgt = fromMaybe (StableAddress tgt) (M.lookup tgt symAddrMap)
          -- | Register containing index for 

concretizeMacawAddr :: forall arch .
  (MM.MemWidth (MM.ArchAddrWidth arch))
  => MM.MemSegmentOff (MM.ArchAddrWidth arch)
  -> Maybe (ConcreteAddress arch)
concretizeMacawAddr x = concreteFromAbsolute <$> mw
  where mw :: Maybe (MM.MemWord (MM.ArchAddrWidth arch))
        mw = MM.segoffAsAbsoluteAddr x


concretizeMacawStmtAddr :: forall arch .
  (MM.MemWidth (MM.ArchAddrWidth arch))
  => MM.MemSegmentOff (MM.ArchAddrWidth arch)
  -- ^ The containing block's start address.
  -> MM.MemWord (MM.ArchAddrWidth arch)
  -- ^ The statements offset from the block start.
  -> Maybe (ConcreteAddress arch)
concretizeMacawStmtAddr y0 yOffset = concreteFromAbsolute <$> MM.asAbsoluteAddr y
  where y :: MM.MemAddr (MM.ArchAddrWidth arch)
        y = MM.segmentOffAddr segment newOffset
        segment :: MM.MemSegment (MM.ArchAddrWidth arch)
        segment = MM.segoffSegment y0
        newOffset :: MM.MemWord (MM.ArchAddrWidth arch)
        newOffset = (MM.segoffOffset y0) + yOffset

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
