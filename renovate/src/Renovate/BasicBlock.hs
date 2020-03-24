{-# LANGUAGE FlexibleContexts #-}
-- | Tools for working with 'BasicBlock's
--
-- This includes functions to convert between concrete and symbolic
-- blocks, tools to compute the sizes of blocks, as well as type
-- definitions.
module Renovate.BasicBlock (
  -- * Blocks
  -- ** Concrete blocks
  ConcreteBlock,
  concreteBlock,
  concreteBlockAddress,
  concreteBlockInstructions,
  concreteDiscoveryBlock,
  concreteBlockSize,
  -- * Symbolic blocks
  SymbolicBlock,
  symbolicBlock,
  symbolicBlockOriginalAddress,
  symbolicBlockSymbolicAddress,
  symbolicBlockInstructions,
  symbolicBlockSize,
  -- ** Padding blocks
  PaddingBlock,
  paddingBlock,
  paddingBlockAddress,
  paddingBlockInstructions,
  -- ** Fallthrough blocks
  FallthroughBlock,
  fallthroughBlock,
  fallthroughOriginalAddress,
  fallthroughSymbolicAddress,
  fallthroughInstructions,
  FallthroughInstruction(..),
  SymbolicFallthrough,
  ConcreteFallthrough,
  addFallthrough,
  noFallthrough,
  FallthroughType(..),
  hasNoAddresses,
  -- ** Concretized blocks
  ConcretizedBlock,
  concretizedBlock,
  concretizedBlockAddress,
  concretizedBlockInstructions,
  concretizedBlockSize,
  -- * Instructions
  Instruction,
  InstructionAnnotation,
  ToGenericInstruction(..),
  RegisterType,
  AddressAssignedBlock(..),
  SymbolicInfo(..),
  instructionStreamSize,
  instructionAddresses,
  instructionAddresses',
  terminatorType,
  TaggedInstruction,
  tagInstruction,
  symbolicTarget,
  projectInstruction,

  -- * Pretty-printers
  prettyConcreteBlock,
  prettyConcretizedBlock,
  -- * Constraints
  InstructionConstraints
  ) where

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Text.Prettyprint.Doc as PD
import qualified Data.Traversable as T
import           Data.Word ( Word64 )
import           GHC.Stack ( HasCallStack )

import qualified Data.Macaw.CFG as MC

import           Renovate.Address
import           Renovate.BasicBlock.Types
import           Renovate.ISA

-- | Compute the addresses for each instruction in a 'BasicBlock'.
--
-- We cannot simply make a @Map a Address@ because two identical
-- instructions could easily occur within the same 'BasicBlock', to
-- say nothing of the entire program.
instructionAddresses :: (MC.MemWidth (MC.ArchAddrWidth arch))
                     => ISA arch
                     -> ConcreteBlock arch
                     -> DLN.NonEmpty (Instruction arch (), ConcreteAddress arch)
instructionAddresses isa bb =
  instructionAddresses' isa id (concreteBlockAddress bb) (concreteBlockInstructions bb)

-- | Compute the addresses of each instruction in a list, given a
-- concrete start address.
--
-- This variant is useful when computing the addresses of instructions
-- in a symbolic block with a known desired start address (e.g., in
-- 'concretize').
instructionAddresses' :: (MC.MemWidth (MC.ArchAddrWidth arch)
                         , T.Traversable t
                         )
                      => ISA arch
                      -> (x -> Instruction arch ())
                      -> ConcreteAddress arch
                      -> t x
                      -> t (x, ConcreteAddress arch)
instructionAddresses' isa accessor startAddr insns =
  snd $ T.mapAccumL computeAddress startAddr insns
  where
    addressAddOffset' = addressAddOffset
    computeAddress addr instr =
      let absAddr = addr `addressAddOffset'` fromIntegral (isaInstructionSize isa (accessor instr))
      in (absAddr, (instr, addr))

-- | Compute the size of a list of instructions, in bytes.
instructionStreamSize :: (Functor f, F.Foldable f) => ISA arch -> f (Instruction arch t) -> Word64
instructionStreamSize isa insns =
  sum $ fmap (fromIntegral . isaInstructionSize isa) insns

-- | Compute the size of a 'ConcreteBlock' in bytes.
concreteBlockSize :: ISA arch -> ConcreteBlock arch -> Word64
concreteBlockSize isa = instructionStreamSize isa . F.toList . concreteBlockInstructions

concretizedBlockSize :: ISA arch -> ConcretizedBlock arch -> Word64
concretizedBlockSize isa = instructionStreamSize isa . F.toList . concretizedBlockInstructions

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
symbolicBlockSize :: (HasCallStack, MC.MemWidth (MC.ArchAddrWidth arch))
                  => ISA arch
                  -> MC.Memory (MC.ArchAddrWidth arch)
                  -> ConcreteAddress arch
                  -> FallthroughBlock arch
                  -> Word64
symbolicBlockSize isa mem addr fb = basicInstSize + fromIntegral jumpSizes
  where
    origAddr = fallthroughOriginalAddress fb
    jumpSizes = sum $ map (computeRewrittenJumpSize isa mem origAddr addr) jumpsToRewrite
    basicInstSize = sum (map (fromIntegral . isaInstructionSize isa . isaConcretizeAddresses isa mem addr . ftInstruction) standardInstructions)
    (standardInstructions, jumpsToRewrite) = DLN.partition hasNoAddresses (fallthroughInstructions fb)

computeRewrittenJumpSize ::
  (HasCallStack, MC.MemWidth (MC.ArchAddrWidth arch)) =>
  ISA arch ->
  MC.Memory (MC.ArchAddrWidth arch) ->
  ConcreteAddress arch ->
  ConcreteAddress arch ->
  FallthroughInstruction arch (SymbolicAddress arch) (InstructionAnnotation arch) ->
  Int
computeRewrittenJumpSize isa mem origAddr addr ftJmp = case rewrittenSize of
  Just size -> size
  Nothing ->
      error ("computeRewrittenJumpSize: Jump cannot be modified: " ++ isaPrettyInstruction isa jmp ++ " at " ++ show addr ++ " at original address " ++ show origAddr ++ " with new target " ++ show (fallthroughType ftJmp))
  where
    instrSize = fromIntegral . isaInstructionSize isa
    jmp = isaConcretizeAddresses isa mem addr (ftInstruction ftJmp)
    fakeTgt = addressAddOffset addr (fromIntegral (isaMaxRelativeJumpSize isa))
    rewrittenJmp = isaModifyJumpTarget isa addr FallthroughInstruction
      { ftInstruction = jmp
      , fallthroughType = fakeTgt <$ fallthroughType ftJmp
      }
    rewrittenSize = sum . map instrSize <$> rewrittenJmp

-- | Return the 'JumpType' of the terminator instruction (if any)
--
-- Note that blocks cannot be empty
terminatorType :: (MC.MemWidth (MC.ArchAddrWidth arch)) => ISA arch -> MC.Memory (MC.ArchAddrWidth arch) -> ConcreteBlock arch -> JumpType arch
terminatorType isa mem b =
  let (termInsn, addr) = DLN.last (instructionAddresses isa b)
  in isaJumpType isa termInsn mem addr

prettyConcreteBlock :: (MC.MemWidth (MC.ArchAddrWidth arch)) => ISA arch -> ConcreteBlock arch -> PD.Doc ann
prettyConcreteBlock isa b =
  PD.vsep [ PD.pretty (concreteBlockAddress b) PD.<> PD.pretty ":"
          , PD.indent 2 (PD.vsep (map (PD.pretty . isaPrettyInstruction isa) (F.toList (concreteBlockInstructions b))))
          ]

prettyConcretizedBlock :: (MC.MemWidth (MC.ArchAddrWidth arch)) => ISA arch -> ConcretizedBlock arch -> PD.Doc ann
prettyConcretizedBlock isa b =
  PD.vsep [ PD.pretty (concretizedBlockAddress b) PD.<> PD.pretty ":"
          , PD.indent 2 (PD.vsep (map (PD.pretty . isaPrettyInstruction isa) (F.toList (concretizedBlockInstructions b))))
          ]
