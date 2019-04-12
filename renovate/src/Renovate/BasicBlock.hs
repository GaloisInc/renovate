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
  Instruction,
  InstructionAnnotation,
  ToGenericInstruction(..),
  RegisterType,
  AddressAssignedBlock(..),
  SymbolicInfo(..),
  concreteBlockSize,
  symbolicBlockSize,
  instructionStreamSize,
  instructionAddresses,
  instructionAddresses',
  terminatorType,
  TaggedInstruction,
  tagInstruction,
  symbolicTarget,
  projectInstruction,
  FallthroughInstruction(..),
  SymbolicFallthrough,
  ConcreteFallthrough,
  addFallthrough,
  noFallthrough,
  hasNoAddresses,
  FallthroughTag(..),
  FallthroughBlock,
  prettyBasicBlock,
  -- * Constraints
  InstructionConstraints
  ) where

import qualified GHC.Err.Located as L

import qualified Data.List as L
import qualified Data.Text.Prettyprint.Doc as PD
import qualified Data.Traversable as T
import           Data.Word ( Word64 )

import qualified Data.Macaw.CFG as MC

import           Renovate.Address
import           Renovate.BasicBlock.Types
import           Renovate.ISA

-- | Compute the addresses for each instruction in a 'BasicBlock'.
--
-- We cannot simply make a @Map a Address@ because two identical
-- instructions could easily occur within the same 'BasicBlock', to
-- say nothing of the entire program.
instructionAddresses :: (MC.MemWidth (MC.ArchAddrWidth arch)) => ISA arch -> ConcreteBlock arch -> [(Instruction arch (), ConcreteAddress arch)]
instructionAddresses isa bb =
  instructionAddresses' isa id (basicBlockAddress bb) (basicBlockInstructions bb)

-- | Compute the addresses of each instruction in a list, given a
-- concrete start address.
--
-- This variant is useful when computing the addresses of instructions
-- in a symbolic block with a known desired start address (e.g., in
-- 'concretize').
instructionAddresses' :: (MC.MemWidth (MC.ArchAddrWidth arch))
                      => ISA arch
                      -> (x -> Instruction arch ())
                      -> ConcreteAddress arch
                      -> [x]
                      -> [(x, ConcreteAddress arch)]
instructionAddresses' isa accessor startAddr insns =
  snd $ T.mapAccumL computeAddress startAddr insns
  where
    addressAddOffset' = addressAddOffset
    computeAddress addr instr =
      let absAddr = addr `addressAddOffset'` fromIntegral (isaInstructionSize isa (accessor instr))
      in (absAddr, (instr, addr))

-- | Compute the size of a list of instructions, in bytes.
instructionStreamSize :: ISA arch -> [Instruction arch t] -> Word64
instructionStreamSize isa insns =
  sum $ map (fromIntegral . isaInstructionSize isa) insns

-- | Compute the size of a 'ConcreteBlock' in bytes.
concreteBlockSize :: ISA arch -> ConcreteBlock arch -> Word64
concreteBlockSize isa = instructionStreamSize isa . basicBlockInstructions

-- | Given a 'ConcreteBlock', compute its size *after* its jump(s) are
-- rewritten.
--
-- We find this size by rewriting the jumps with fake addresses (since
-- we don't know the real addresses yet).  For each jump, we put the target as
-- far away as possible to guarantee that the ISA gives us the worst-case
-- instructions in terms of assembled size. Later we will rewrite the jumps
-- with real addresses, then insert padding to fill out any unused space.
--
-- The address parameter is a dummy used as a stand in for the address of jump
-- destinations.
symbolicBlockSize :: (L.HasCallStack, MC.MemWidth (MC.ArchAddrWidth arch))
                  => ISA arch
                  -> MC.Memory (MC.ArchAddrWidth arch)
                  -> ConcreteAddress arch
                  -> FallthroughBlock arch
                  -> Word64
symbolicBlockSize isa mem addr fb = basicInstSize + fromIntegral jumpSizes
  where
    origAddr = concreteAddress (basicBlockAddress fb)
    jumpSizes = sum $ map (computeRewrittenJumpSize isa mem origAddr addr) jumpsToRewrite
    basicInstSize = sum (map (fromIntegral . isaInstructionSize isa . isaConcretizeAddresses isa mem addr . ftInstruction) standardInstructions)
    (standardInstructions, jumpsToRewrite) = L.partition hasNoAddresses (basicBlockInstructions fb)

computeRewrittenJumpSize ::
  (L.HasCallStack, MC.MemWidth (MC.ArchAddrWidth arch)) =>
  ISA arch ->
  MC.Memory (MC.ArchAddrWidth arch) ->
  ConcreteAddress arch ->
  ConcreteAddress arch ->
  SymbolicFallthrough arch (InstructionAnnotation arch) ->
  Int
computeRewrittenJumpSize isa mem origAddr addr ftJmp = case rewrittenSize of
  Just size -> size
  Nothing ->
      error ("computeRewrittenJumpSize: Jump cannot be modified: " ++ isaPrettyInstruction isa jmp ++ " at " ++ show addr ++ " at original address " ++ show origAddr ++ " with new target " ++ show (ftTag ftJmp))
  where
    instrSize = fromIntegral . isaInstructionSize isa
    jmp = isaConcretizeAddresses isa mem addr (ftInstruction ftJmp)
    fakeTgt = addressAddOffset addr (fromIntegral (isaMaxRelativeJumpSize isa))
    rewrittenJmp = isaModifyJumpTarget isa addr FallthroughInstruction
      { ftInstruction = jmp
      , ftTag = fakeTgt <$ ftTag ftJmp
      }
    rewrittenSize = sum . map instrSize <$> rewrittenJmp

-- | Return the 'JumpType' of the terminator instruction (if any)
--
-- If the block is empty, it will return 'NoJump'.
terminatorType :: (MC.MemWidth (MC.ArchAddrWidth arch)) => ISA arch -> MC.Memory (MC.ArchAddrWidth arch) -> ConcreteBlock arch -> JumpType arch
terminatorType isa mem b =
  case instructionAddresses isa b of
    [] -> NoJump
    insns ->
      let (termInsn, addr) = last insns
      in isaJumpType isa termInsn mem addr

prettyBasicBlock :: (PD.Pretty addr) => ISA arch -> BasicBlock addr (Instruction arch) a -> PD.Doc ann
prettyBasicBlock isa b =
  PD.vsep [ PD.pretty (basicBlockAddress b) PD.<> PD.pretty ":"
          , PD.indent 2 (PD.vsep (map (PD.pretty . isaPrettyInstruction isa) (basicBlockInstructions b)))
          ]
