{-# LANGUAGE FlexibleContexts #-}
-- | Tools for working with 'BasicBlock's
--
-- This includes functions to convert between concrete and symbolic
-- blocks, tools to compute the sizes of blocks, as well as type
-- definitions.
module Renovate.BasicBlock (
  BasicBlock(..),
  ConcreteBlock,
  SymbolicBlock,
  AddressAssignedBlock(..),
  SymbolicInfo(..),
  concreteBlockSize,
  symbolicBlockSize,
  instructionStreamSize,
  instructionAddresses,
  instructionAddresses',
  TaggedInstruction,
  tag,
  tagInstruction,
  hasNoSymbolicTarget,
  symbolicTarget,
  projectInstruction
  ) where

import qualified GHC.Err.Located as L

import qualified Data.List as L
import qualified Data.Traversable as T
import           Data.Word ( Word64 )

import qualified Data.Macaw.Memory as MC

import           Renovate.Address
import           Renovate.BasicBlock.Types
import           Renovate.ISA

-- | Compute the addresses for each instruction in a 'BasicBlock'.
--
-- We cannot simply make a @Map a Address@ because two identical
-- instructions could easily occur within the same 'BasicBlock', to
-- say nothing of the entire program.
instructionAddresses :: (MC.MemWidth w) => ISA i a w -> MC.Memory w -> ConcreteBlock i w -> [(i (), RelAddress w)]
instructionAddresses isa mem bb =
  instructionAddresses' isa mem id (basicBlockAddress bb) (basicBlockInstructions bb)

-- | Compute the addresses of each instruction in a list, given a
-- concrete start address.
--
-- This variant is useful when computing the addresses of instructions
-- in a symbolic block with a known desired start address (e.g., in
-- 'concretize').
instructionAddresses' :: (MC.MemWidth w)
                      => ISA i a w
                      -> MC.Memory w
                      -> (x -> i ())
                      -> RelAddress w
                      -> [x]
                      -> [(x, RelAddress w)]
instructionAddresses' isa mem accessor startAddr insns =
  snd $ T.mapAccumL computeAddress startAddr insns
  where
    addressAddOffset' = addressAddOffset mem
    computeAddress addr instr =
      let absAddr = addr `addressAddOffset'` fromIntegral (isaInstructionSize isa (accessor instr))
      in (absAddr, (instr, addr))

-- | Compute the size of a list of instructions, in bytes.
instructionStreamSize :: ISA i a w -> [i t] -> Word64
instructionStreamSize isa insns =
  sum $ map (fromIntegral . isaInstructionSize isa) insns

-- | Compute the size of a 'ConcreteBlock' in bytes.
concreteBlockSize :: ISA i a w -> ConcreteBlock i w -> Word64
concreteBlockSize isa = instructionStreamSize isa . basicBlockInstructions

-- | Given a 'ConcreteBlock', compute its size *after* its jump(s) are
-- rewritten.
--
-- We find this size by rewriting the jumps with fake addresses (since
-- we don't know the real addresses yet).  For each jump, we always
-- generate the same size of instruction independent of the size of
-- the operand, so this is safe.
--
-- The address parameter is a dummy used as a stand in for the address of jump
-- destinations.
symbolicBlockSize :: (L.HasCallStack, MC.MemWidth w)
                  => ISA i a w
                  -> MC.Memory w
                  -> RelAddress w
                  -> SymbolicBlock i a w
                  -> Word64
symbolicBlockSize isa mem addr sb = basicInstSize + fromIntegral jumpSizes
  where
    jumpSizes = sum $ map (computeRewrittenJumpSize isa mem addr . projectInstruction) jumpsToRewrite
    basicInstSize = sum (map (fromIntegral . isaInstructionSize isa . isaConcretizeAddresses isa mem addr . projectInstruction) standardInstructions)
    (standardInstructions, jumpsToRewrite) = L.partition hasNoSymbolicTarget (basicBlockInstructions sb)

computeRewrittenJumpSize :: (L.HasCallStack) => ISA i a w -> MC.Memory w -> RelAddress w -> i a -> Int
computeRewrittenJumpSize isa mem addr jmp =
  case isaModifyJumpTarget isa (isaConcretizeAddresses isa mem addr jmp) addr addr of
    Nothing -> L.error ("computeRewrittenJumpSize: Not a jump: " ++ isaPrettyInstruction isa jmp)
    Just ji -> sum (map (fromIntegral . isaInstructionSize isa) ji)
