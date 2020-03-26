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
  -- ** Explicit fallthrough blocks
  ExplicitFallthroughBlock,
  explicitFallthroughBlock,
  explicitFallthroughAddress,
  explicitFallthroughInstructions,
  explicitFallthroughSuccessor,
  -- * Symbolic blocks
  SymbolicBlock,
  symbolicBlock,
  symbolicBlockOriginalAddress,
  symbolicBlockSymbolicAddress,
  symbolicBlockInstructions,
  symbolicBlockSymbolicSuccessor,
  symbolicBlockSize,
  symbolicBlockWithoutSuccessor,
  -- ** Padding blocks
  PaddingBlock,
  paddingBlock,
  paddingBlockAddress,
  paddingBlockInstructions,
  -- ** Concretized blocks
  ConcretizedBlock,
  concretizedBlock,
  concretizedBlockAddress,
  concretizedBlockInstructions,
  -- ** Concrete block helpers
  HasConcreteAddresses,
  blockAddress,
  blockSize,
  instructionAddresses,
  instructionAddresses',
  -- * Instructions
  Instruction,
  InstructionAnnotation,
  ToGenericInstruction(..),
  RegisterType,
  AddressAssignedBlock(..),
  SymbolicInfo(..),
  instructionStreamSize,
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
import           Data.Maybe ( fromMaybe )
import qualified Data.Text.Prettyprint.Doc as PD
import qualified Data.Traversable as T
import           Data.Word ( Word64 )
import           GHC.Stack ( HasCallStack )

import qualified Data.Macaw.CFG as MC

import           Renovate.Address
import           Renovate.BasicBlock.Types
import           Renovate.ISA
import qualified Renovate.Panic as RP

-- | Functions that work on any block that has unannotated instructions and concrete addresses
class HasConcreteAddresses b where
  -- | Compute the addresses for each instruction in a 'BasicBlock'.
  --
  -- We cannot simply make a @Map a Address@ because two identical
  -- instructions could easily occur within the same 'BasicBlock', to
  -- say nothing of the entire program.
  instructionAddresses :: (MC.MemWidth (MC.ArchAddrWidth arch))
                       => ISA arch
                       -> b arch
                       -> DLN.NonEmpty (Instruction arch (), ConcreteAddress arch)
  -- | Compute the size of a block in bytes.
  blockSize :: ISA arch -> b arch -> Word64

  blockAddress :: b arch -> ConcreteAddress arch

instance HasConcreteAddresses ConcreteBlock where
  instructionAddresses isa bb =
    instructionAddresses' isa id (concreteBlockAddress bb) (concreteBlockInstructions bb)
  blockSize isa = instructionStreamSize isa . F.toList . concreteBlockInstructions
  blockAddress = concreteBlockAddress

instance HasConcreteAddresses ConcretizedBlock where
  instructionAddresses isa bb =
    instructionAddresses' isa id (concretizedBlockAddress bb) (concretizedBlockInstructions bb)
  blockSize isa = instructionStreamSize isa . F.toList . concretizedBlockInstructions
  blockAddress = concretizedBlockAddress

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
    computeAddress addr instr =
      let absAddr = addr `addressAddOffset` fromIntegral (isaInstructionSize isa (accessor instr))
      in (absAddr, (instr, addr))

-- | Compute the size of a list of instructions, in bytes.
instructionStreamSize :: (Functor f, F.Foldable f) => ISA arch -> f (Instruction arch t) -> Word64
instructionStreamSize isa insns =
  sum $ fmap (fromIntegral . isaInstructionSize isa) insns

-- | Given a 'SymbolicBlock', compute its size after all of its jumps are
-- concretized and any fallthrough code is added.  This is an upper-bound size,
-- where some of those bytes could be turned into no-ops (e.g., if the rewriter
-- is not in a mode where it needs to reify control flow fallthroughs with
-- explicit jumps).
--
-- The fallthrough jump, if any, will be placed after the last instruction in
-- the block.  We need to compute all of the rewritten jumps, in case the user
-- has inserted some jumps of their own.  However, we only need to account for
-- the fallthrough target with the last instruction.  Thus, we'll peel off the
-- last instruction and treat it specially (computing its modified jump using
-- the fallthrough successor of the block), while simply saying that the
-- remaining jumps have no fallthrough successor.
--
-- NOTE: Each 'SymbolicBlock' can only have one possible fallthrough, which was
-- computed earlier.  Fallthroughs are only patched in at the end of a block.
-- If client code wishes to avoid the fallthrough behavior, it can always add an
-- unconditional jump that skips the automatically-inserted fallthrough.
--
-- FIXME: We could re-analyze the code to determine if that is the case and
-- completely elide any fallthrough code.  It is low-priority though, since the
-- savings are small and that seems like an uncommon scenario.
--
-- NOTE: The implementation of this computation MUST match the actual rewriting
-- performed in 'concretizeJumps' in Renovate.Redirect.Concretize.
symbolicBlockSize :: (HasCallStack, MC.MemWidth (MC.ArchAddrWidth arch))
                  => ISA arch
                  -> MC.Memory (MC.ArchAddrWidth arch)
                  -> ConcreteAddress arch
                  -> SymbolicBlock arch
                  -> Word64
symbolicBlockSize isa mem addr sb =
  fromIntegral (basicInstSize + jumpSizes + fallthroughSize)
  where
    origAddr = symbolicBlockOriginalAddress sb

    instrs0 = fmap projectInstruction (symbolicBlockInstructions sb)
    (standardInstructions, jumps) = DLN.partition (isNotJumpInstruction isa mem fakeTarget) instrs0

    basicInstSize = sum $ fmap (fromIntegral . isaInstructionSize isa . isaConcretizeAddresses isa mem addr) standardInstructions
    concreteJumps = fmap (isaConcretizeAddresses isa mem origAddr) jumps
    jumpSizes = sum $ fmap (computeJumpSize isa origAddr addr) concreteJumps

    fallthroughSize = fromMaybe 0 $ do
      -- Guard; fail if there is no symbolic successor (and then return 0 via
      -- the fromMaybe)
      _ <- symbolicBlockSymbolicSuccessor sb
      let fallthroughJump = isaMakeRelativeJumpTo isa origAddr fakeTarget
      return $ sum $ fmap (computeJumpSize isa origAddr addr) fallthroughJump

    -- We don't have the real fallthrough address because it hasn't been
    -- allocated yet.  We can substitute in a fake value here to compute the
    -- size that the jump will ultimately require
    fakeTarget = addr `addressAddOffset` fromIntegral (isaMaxRelativeJumpSize isa)

-- | Return 'False' if this is an instruction that needs to be subject to 'isaModifyJumpTarget'
--
-- We elide instructions that are not control flow instructions.  We also elide
-- returns, as they do not need to be fixed up.  Likewise indirect calls; while
-- they return, there is no fixup necessary for indirect calls that are not
-- terminal in blocks (terminal indirect calls may need to have a fallthrough
-- jump inserted).
--
-- NOTE: If an indirect jump makes it this far, we are not rewriting it and have
-- no way to fix it up.  In fact, it would be an error to see one here, as no
-- blocks with indirect jumps remaining should be modified.
isNotJumpInstruction :: ISA arch -> MC.Memory (MC.ArchAddrWidth arch) -> ConcreteAddress arch -> Instruction arch a -> Bool
isNotJumpInstruction isa mem addr i =
  case isaJumpType isa i mem addr of
    NoJump -> True
    Return {} -> True
    RelativeJump {} -> False
    AbsoluteJump {} -> False
    DirectCall {} -> False
    IndirectCall {} -> False
    IndirectJump {} ->
      RP.panic RP.BasicBlockSize "isNotJumpInstruction" [ "Indirect jumps should not be relocated at this point: "
                                                        , "  " ++ isaPrettyInstruction isa i
                                                        ]

-- | Compute the size of a jump instruction (accounting for its ultimate target
-- and any necessary reified fallthrough).
--
-- NOTE: The fallthrough address, if provided, is a faked one that simply allows
-- us to compute the correct instruction sequence sizes.
computeJumpSize :: ( HasCallStack
                   , MC.MemWidth (MC.ArchAddrWidth arch)
                   )
                => ISA arch
                -> ConcreteAddress arch
                -> ConcreteAddress arch
                -> Instruction arch ()
                -> Int
computeJumpSize isa origAddr addr insn =
  -- Add any necessary fallthroughs; this step requires a concrete
  -- instruction, which is why we concretized first
  case isaModifyJumpTarget isa addr insn of
    Nothing ->
      RP.panic RP.BasicBlockSize "computeJumpSize" [ "Jump cannot be modified: "
                                                   , "  Instruction: " ++ isaPrettyInstruction isa insn
                                                   , "  Original Address: " ++ show origAddr
                                                   ]
    Just jmpSeq -> sum (fmap instrSize jmpSeq)
  where
    instrSize = fromIntegral . isaInstructionSize isa

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
