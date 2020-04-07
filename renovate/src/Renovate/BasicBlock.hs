{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
-- | Tools for working with 'BasicBlock's
--
-- This includes functions to convert between concrete and symbolic
-- blocks, tools to compute the sizes of blocks, as well as type
-- definitions.
module Renovate.BasicBlock (
  -- * Blocks
  -- ** Concrete blocks
  ConcreteBlock(..),
  concreteBlock,
  withConcreteInstructions,
  -- ** Symbolic blocks
  SymbolicBlock,
  symbolicBlock,
  symbolicBlockOriginalAddress,
  symbolicBlockSymbolicAddress,
  symbolicBlockSymbolicSuccessor,
  symbolicBlockSize,
  symbolicBlockWithoutSuccessor,
  withSymbolicInstructions,
  -- *** Additional symbolic address support
  TaggedInstruction,
  tagInstruction,
  symbolicTarget,
  projectInstruction,
  RelocatableTarget(..),
  HasNoTarget,
  HasSomeTarget,
  -- ** Padding blocks
  PaddingBlock,
  paddingBlock,
  paddingBlockAddress,
  withPaddingInstructions,
  -- ** Concretized blocks
  ConcretizedBlock,
  concretizedBlock,
  concretizedBlockAddress,
  withConcretizedInstructions,
  -- ** Concrete block helpers
  HasConcreteAddresses,
  blockAddress,
  blockSize,
  withInstructionAddresses,
  instructionAddresses',
  -- * Instructions
  InstructionArchRepr,
  InstructionArchReprKind,
  SomeInstructionArchRepr(..),
  Instruction,
  InstructionAnnotation,
  ToGenericInstruction(..),
  RegisterType,
  AddressAssignedBlock(..),
  SymbolicInfo(..),
  symbolicInfo,
  instructionStreamSize,
  terminatorType,

  -- * Pretty-printers
  prettyConcreteBlock,
  prettyConcretizedBlock,
  -- * Constraints
  ArchConstraints,
  InstructionConstraints
  ) where

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import           Data.Maybe ( fromMaybe )
import           Data.Parameterized.Some ( Some(..) )
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
  withInstructionAddresses :: (MC.MemWidth (MC.ArchAddrWidth arch))
                           => ISA arch
                           -> b arch
                           -> (forall (tp :: InstructionArchReprKind arch)
                               . ( ArchConstraints arch tp )
                                => InstructionArchRepr arch tp -> DLN.NonEmpty (Instruction arch tp (), ConcreteAddress arch) -> a)
                           -> a
  -- | Compute the size of a block in bytes.
  blockSize :: ISA arch -> b arch -> Word64

  blockAddress :: b arch -> ConcreteAddress arch

instance HasConcreteAddresses ConcreteBlock where
  withInstructionAddresses isa (ConcreteBlock addr insns repr _) k =
    k repr (instructionAddresses' isa id addr insns)
  blockSize isa (ConcreteBlock _ insns _ _) =
    instructionStreamSize isa (F.toList insns)
  blockAddress = concreteBlockAddress

instance HasConcreteAddresses ConcretizedBlock where
  withInstructionAddresses isa (ConcretizedBlock addr insns repr) k =
    k repr (instructionAddresses' isa id addr insns)
  blockSize isa (ConcretizedBlock _ insns _) =
    instructionStreamSize isa (F.toList insns)
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
                      -> (x -> Instruction arch tp ())
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
instructionStreamSize :: (Functor f, F.Foldable f) => ISA arch -> f (Instruction arch tp t) -> Word64
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
-- has inserted some jumps of their own.
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
                  -> SymbolicBlock arch
                  -> Word64
symbolicBlockSize isa mem (SymbolicBlock origAddr _symAddr insns repr mSymSucc) =
  fromIntegral (normalInstSizes + jumpSizes + fallthroughInstSizes)
  where
    -- The symbolic block has tagged instructions, which have each modifiable
    -- control flow transfer instruction tagged with a 'RelocatableTarget',
    -- which must be fixed up with 'isaModifyJumpTarget'.  We start by
    -- partitioning into regular instructions that don't need to be fixed and
    -- those that do.
    (normalInstructions, jumpInstructions) =
      foldr partitionJumps ([], []) insns

    -- We make a concrete sequence of instructions for the fallthrough code.
    -- This is generated to be concrete, so we can just take its size trivially
    -- without calling 'isaModifyJumpTarget', as it is already the correct
    -- instruction sequence.  The actual target is not very relevant, as
    -- 'isaModifyJumpTarget' must always produce the same size jump regardless
    -- of the ultimate target.
    normalInstSizes =
      sum (fmap (fromIntegral . isaInstructionSize isa . projectInstruction) normalInstructions)
    jumpSizes = sum (fmap (computeJumpSize isa mem origAddr) jumpInstructions)

    -- Determine what (if any) sequence of instructions we need to add to handle
    -- control flow fallthrough.
    fallthroughInstrSeq = fromMaybe [] $ do
      -- Guard; fail if there is no symbolic successor (and then return 0 via
      -- the fromMaybe)
      _ <- mSymSucc
      return (F.toList (isaMakeRelativeJumpTo isa origAddr origAddr repr))
    fallthroughInstSizes =
      sum (fmap (fromIntegral . isaInstructionSize isa) fallthroughInstrSeq)

partitionJumps :: TaggedInstruction arch tp (InstructionAnnotation arch)
               -> ([TaggedInstruction arch tp (InstructionAnnotation arch)]
                  ,[( TaggedInstruction arch tp (InstructionAnnotation arch)
                    , RelocatableTarget arch SymbolicAddress HasSomeTarget
                    )
                   ]
                  )
               -> ([TaggedInstruction arch tp (InstructionAnnotation arch)]
                  ,[( TaggedInstruction arch tp (InstructionAnnotation arch)
                    , RelocatableTarget arch SymbolicAddress HasSomeTarget
                    )
                   ]
                  )
partitionJumps i (normalInstructions, jumpInstructions) =
  case symbolicTarget i of
    Some NoTarget ->
      (i : normalInstructions, jumpInstructions)
    Some (rt@RelocatableTarget {}) ->
      (normalInstructions, (i, rt) : jumpInstructions)

computeJumpSize :: (MC.MemWidth (MC.ArchAddrWidth arch))
                => ISA arch
                -> MC.Memory (MC.ArchAddrWidth arch)
                -> ConcreteAddress arch
                -> ( TaggedInstruction arch tp (InstructionAnnotation arch)
                   , RelocatableTarget arch SymbolicAddress HasSomeTarget
                   )
                -> Int
computeJumpSize isa mem addr (taggedInstr, RelocatableTarget target) =
  case isaModifyJumpTarget isa addr insn fakeTarget of
    Nothing ->
      RP.panic RP.BasicBlockSize "computeJumpSize" [ "Jump cannot be modified: "
                                                   , "  Instruction: " ++ isaPrettyInstruction isa insn
                                                   , "  Original Address: " ++ show addr
                                                   , "  Symbolic Target: " ++ show target
                                                   ]
    Just jmpSeq -> sum (fmap (fromIntegral . isaInstructionSize isa) jmpSeq)
  where
    insn = isaConcretizeAddresses isa mem addr (projectInstruction taggedInstr)
    -- We don't know the actual address of the target yet since we haven't done
    -- layout, but we need something to pass in so we use a fake address.
    fakeTarget = RelocatableTarget addr


-- | Return the 'JumpType' of the terminator instruction (if any)
--
-- Note that blocks cannot be empty
terminatorType :: (MC.MemWidth (MC.ArchAddrWidth arch)) => ISA arch -> MC.Memory (MC.ArchAddrWidth arch) -> ConcreteBlock arch -> Some (JumpType arch)
terminatorType isa mem b =
  withInstructionAddresses isa b $ \_repr insns ->
    let (termInsn, addr) = DLN.last insns
    in case concreteDiscoveryBlock b of
      Some pb -> isaJumpType isa termInsn mem addr pb

prettyConcreteBlock :: (MC.MemWidth (MC.ArchAddrWidth arch)) => ISA arch -> ConcreteBlock arch -> PD.Doc ann
prettyConcreteBlock isa (ConcreteBlock addr insns _repr _pb) =
  PD.vsep [ PD.pretty addr PD.<> PD.pretty ":"
          , PD.indent 2 (PD.vsep (map (PD.pretty . isaPrettyInstruction isa) (F.toList insns)))
          ]

prettyConcretizedBlock :: (MC.MemWidth (MC.ArchAddrWidth arch)) => ISA arch -> ConcretizedBlock arch -> PD.Doc ann
prettyConcretizedBlock isa (ConcretizedBlock addr insns _repr) =
  PD.vsep [ PD.pretty addr PD.<> PD.pretty ":"
          , PD.indent 2 (PD.vsep (map (PD.pretty . isaPrettyInstruction isa) (F.toList insns)))
          ]
