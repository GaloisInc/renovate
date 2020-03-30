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
  -- * Symbolic blocks
  SymbolicBlock,
  symbolicBlock,
  symbolicBlockOriginalAddress,
  symbolicBlockSymbolicAddress,
  symbolicBlockSymbolicSuccessor,
  symbolicBlockSize,
  symbolicBlockWithoutSuccessor,
  withSymbolicInstructions,
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
                               . ( MC.MemWidth (MC.ArchAddrWidth arch)
                                 , Show (Instruction arch tp ())
                                 , PD.Pretty (Instruction arch tp ())
                                 )
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
                  -> ConcreteAddress arch
                  -> SymbolicBlock arch
                  -> Word64
symbolicBlockSize isa mem addr (SymbolicBlock origAddr _symAddr insns repr mSymSucc) =
  fromIntegral (basicInstSize + jumpSizes)
  where
    instrs0 = fmap projectInstruction insns -- (symbolicBlockInstructions sb)
    concreteInstrs = fmap (isaConcretizeAddresses isa mem origAddr) instrs0

    -- Determine what (if any) sequence of instructions we need to add to handle
    -- control flow fallthrough.
    fallthroughInstrSeq = fromMaybe [] $ do
      -- Guard; fail if there is no symbolic successor (and then return 0 via
      -- the fromMaybe)
      _ <- mSymSucc
      return (F.toList (isaMakeRelativeJumpTo isa origAddr origAddr repr))

    -- Split the instructions into normal instructions and jump instructions
    -- that need to be modified (while keeping around the type-level evidence
    -- that the jumps are actually jumps)
    (standardInstructions, taggedJumps) =
      foldr (partitionModifiableJumps isa mem fakeTarget) ([], []) (F.toList concreteInstrs <> fallthroughInstrSeq)

    -- Before processing the standard instructions, we throw away the tags (we
    -- only kept them as a type-level proof that we didn't mix any instruction
    -- types)
    basicInstSize = sum $ fmap (fromIntegral . isaInstructionSize isa) (fmap fst standardInstructions)
    jumpSizes = sum $ fmap (computeJumpSize isa repr origAddr addr) taggedJumps

    -- We don't have the real fallthrough address because it hasn't been
    -- allocated yet.  We can substitute in a fake value here to compute the
    -- size that the jump will ultimately require
    fakeTarget = addr `addressAddOffset` fromIntegral (isaMaxRelativeJumpSize isa repr)

-- | Split instructions into regular instructions and jumps that need to be
-- fixed up, while retaining type-level evidence that the jumps are actually jumps
--
-- NOTE: This function does not maintain the ordering of instructions as
-- 'symbolicBlockSize' only needs to count up sizes.
--
-- NOTE: We keep the jump type results for regular instructions to prove to
-- ourselves that we didn't accidentally include an instruction that could be
-- modified in the regular instructions list (the type-level tags prevent it)
partitionModifiableJumps :: ISA arch
                         -> MC.Memory (MC.ArchAddrWidth arch)
                         -> ConcreteAddress arch
                         -> Instruction arch tp t
                         -> ( [(Instruction arch tp t, JumpType arch NoModifiableTarget)]
                            , [(Instruction arch tp t, JumpType arch HasModifiableTarget)]
                            )
                         -> ( [(Instruction arch tp t, JumpType arch NoModifiableTarget)]
                            , [(Instruction arch tp t, JumpType arch HasModifiableTarget)]
                            )
partitionModifiableJumps isa mem addr instr (regularInstrs, jumpInstrs) =
  case isaJumpType isa instr mem addr of
    Some jt@NoJump -> ((instr, jt) : regularInstrs, jumpInstrs)
    Some (jt@Return {}) -> ((instr, jt) : regularInstrs, jumpInstrs)
    Some (jt@IndirectCall {}) -> ((instr, jt) : regularInstrs, jumpInstrs)
    -- While we shouldn't really see any indirect jumps from the original code
    -- here, it is possible that callers of renovate have inserted their own
    -- indirect jumps.  Thus, we don't panic here (though it is on clients to
    -- ensure that what they added was safe)
    Some (jt@IndirectJump {}) -> ((instr, jt) : regularInstrs, jumpInstrs)
    Some (jt@RelativeJump {}) -> (regularInstrs, (instr, jt) : jumpInstrs)
    Some (jt@AbsoluteJump {}) -> (regularInstrs, (instr, jt) : jumpInstrs)
    Some (jt@DirectCall {}) -> (regularInstrs, (instr, jt) : jumpInstrs)

-- | Compute the size of a jump instruction (accounting for its ultimate target)
--
-- Note that we compute the size using 'isaModifyJumpTarget', which requires a
-- jump target (if the instruction is really a jump).  Thus, we unconditionally
-- provide a (faked) target.
computeJumpSize :: ( HasCallStack
                   , MC.MemWidth (MC.ArchAddrWidth arch)
                   )
                => ISA arch
                -> InstructionArchRepr arch tp
                -> ConcreteAddress arch
                -> ConcreteAddress arch
                -> (Instruction arch tp (), JumpType arch HasModifiableTarget)
                -> Int
computeJumpSize isa repr origAddr addr (insn, jt) =
  -- Add any necessary fallthroughs; this step requires a concrete
  -- instruction, which is why we concretized first
  case isaModifyJumpTarget isa addr insn jt fakeTarget of
    Nothing ->
      RP.panic RP.BasicBlockSize "computeJumpSize" [ "Jump cannot be modified: "
                                                   , "  Instruction: " ++ isaPrettyInstruction isa insn
                                                   , "  Original Address: " ++ show origAddr
                                                   ]
    Just jmpSeq -> sum (fmap instrSize jmpSeq)
  where
    instrSize = fromIntegral . isaInstructionSize isa
    fakeTarget = addr `addressAddOffset` fromIntegral (isaMaxRelativeJumpSize isa repr)

-- | Return the 'JumpType' of the terminator instruction (if any)
--
-- Note that blocks cannot be empty
terminatorType :: (MC.MemWidth (MC.ArchAddrWidth arch)) => ISA arch -> MC.Memory (MC.ArchAddrWidth arch) -> ConcreteBlock arch -> Some (JumpType arch)
terminatorType isa mem b =
  withInstructionAddresses isa b $ \_repr insns ->
    let (termInsn, addr) = DLN.last insns
    in isaJumpType isa termInsn mem addr

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
