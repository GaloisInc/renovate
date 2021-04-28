{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Tools for working with 'BasicBlock's
--
-- This includes functions to convert between concrete and symbolic
-- blocks, tools to compute the sizes of blocks, as well as type
-- definitions.
module Renovate.Core.BasicBlock (
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
  symbolicBlockDiscoveryBlock,
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
  computeInstructionSize,
  AddressAssignedBlock(..),
  SymbolicInfo(..),
  symbolicInfo,
  instructionStreamSize,
  terminatorType,

  -- * Pretty-printers
  prettyConcreteBlock,
  prettyConcretizedBlock,
  ) where

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import           Data.Maybe ( fromMaybe )
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Traversable as T
import           Data.Word ( Word64 )
import           GHC.Stack ( HasCallStack )
import qualified Prettyprinter as PP

import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MD

import qualified Renovate.Core.Address as RCA
import qualified Renovate.Core.Instruction as RCI
import qualified Renovate.Core.Relocation as RCR
import qualified Renovate.ISA as RI


-- | The 'ConcreteBlock' is a basic block (address + non-empty list of
-- instructions) with an associated macaw 'MD.ParsedBlock'.
--
-- These are the blocks produced by the code discovery process (see
-- Recovery.hs).  As they always come from macaw blocks, we have a macaw block
-- to associate with them on a one-to-one basis.
data ConcreteBlock arch =
  forall (tp :: RCI.InstructionArchReprKind arch) .
  ( RCI.InstructionConstraints arch tp ) =>
  ConcreteBlock { concreteBlockAddress :: RCA.ConcreteAddress arch
                , concreteBlockInstructions :: DLN.NonEmpty (RCI.Instruction arch tp ())
                , concreteBlockRepr :: RCI.InstructionArchRepr arch tp
                -- ^ A value-level representative for the sub-type of
                -- instruction we are carrying
                --
                -- The type parameter is quantified out so that we can pass
                -- blocks around without worrying about it
                , concreteDiscoveryBlock :: Some (MD.ParsedBlock arch)
                -- ^ The macaw block that generated this 'ConcreteBlock'
                }

instance Show (ConcreteBlock arch) where
  show (ConcreteBlock addr insns _repr _pb) =
    concat [ "ConcreteBlock "
           , show addr
           , " "
           , show insns
           ]

instance PP.Pretty (ConcreteBlock arch) where
  pretty (ConcreteBlock addr insns _ _) =
    PP.pretty addr PP.<> PP.pretty ":" PP.<+> PP.align (PP.vsep (map PP.pretty (F.toList insns)))


concreteBlock :: forall arch (tp :: RCI.InstructionArchReprKind arch) ids
               . ( RCI.InstructionConstraints arch tp )
              => RCA.ConcreteAddress arch
              -> DLN.NonEmpty (RCI.Instruction arch tp ())
              -> RCI.InstructionArchRepr arch tp
              -> MD.ParsedBlock arch ids
              -> ConcreteBlock arch
concreteBlock addr insns repr pb =
  ConcreteBlock addr insns repr (Some pb)

withConcreteInstructions :: ConcreteBlock arch
                         -> ( forall (tp :: RCI.InstructionArchReprKind arch)
                            . ( RCI.InstructionConstraints arch tp )
                           => RCI.InstructionArchRepr arch tp -> DLN.NonEmpty (RCI.Instruction arch tp ()) -> a)
                         -> a
withConcreteInstructions (ConcreteBlock _addr insns repr _) k =
  k repr insns

-- | This is a basic block that contains padding bytes at a given address.
--
-- These have limited use (mostly in the layout code when we need to place
-- padding between other blocks.  Note that this is not just a number of bytes,
-- as some users want to put meaningful data in padding.
data PaddingBlock arch =
  forall (tp :: RCI.InstructionArchReprKind arch) .
  ( RCI.InstructionConstraints arch tp ) =>
  PaddingBlock { paddingBlockAddress :: RCA.ConcreteAddress arch
               , _paddingBlockInstructions :: DLN.NonEmpty (RCI.Instruction arch tp ())
               , _paddingBlockRepr :: RCI.InstructionArchRepr arch tp
               }

paddingBlock :: forall arch (tp :: RCI.InstructionArchReprKind arch)
              . ( RCI.InstructionConstraints arch tp )
             => RCA.ConcreteAddress arch
             -> DLN.NonEmpty (RCI.Instruction arch tp ())
             -> RCI.InstructionArchRepr arch tp
             -> PaddingBlock arch
paddingBlock = PaddingBlock

withPaddingInstructions :: PaddingBlock arch
                        -> ( forall (tp :: RCI.InstructionArchReprKind arch)
                           . ( RCI.InstructionConstraints arch tp )
                           => RCI.InstructionArchRepr arch tp -> DLN.NonEmpty (RCI.Instruction arch tp ()) -> a)
                        -> a
withPaddingInstructions (PaddingBlock _ insns repr) k =
  k repr insns

-- | The type of 'BasicBlock's that only have symbolic addresses.
-- Their jumps are annotated with symbolic address targets as well,
-- which refer to other 'SymbolicBlock's.
data SymbolicBlock arch =
  forall (tp :: RCI.InstructionArchReprKind arch) .
  ( RCI.InstructionConstraints arch tp ) =>
  SymbolicBlock { symbolicBlockOriginalAddress :: RCA.ConcreteAddress arch
                , symbolicBlockSymbolicAddress :: RCA.SymbolicAddress arch
                , symbolicBlockInstructions :: DLN.NonEmpty (RCI.Instruction arch tp (RCR.Relocation arch))
                , symbolicBlockRepr :: RCI.InstructionArchRepr arch tp
                , symbolicBlockSymbolicSuccessor :: Maybe (RCA.SymbolicAddress arch)
                , symbolicBlockDiscoveryBlock :: Some (MD.ParsedBlock arch)
                }

symbolicBlock :: forall arch (tp :: RCI.InstructionArchReprKind arch)
               . ( RCI.InstructionConstraints arch tp )
              => RCA.ConcreteAddress arch
              -- ^ The 'ConcreteBlock' this 'SymbolicBlock' was lifted from
              -> RCA.SymbolicAddress arch
              -- ^ The symbolic address assigned to this block
              -> DLN.NonEmpty (RCI.Instruction arch tp (RCR.Relocation arch))
              -- ^ Instructions with symbolic address annotations
              -> RCI.InstructionArchRepr arch tp
              -> Maybe (RCA.SymbolicAddress arch)
              -- ^ The symbolic address of the successor, if any
              --
              -- This could be Nothing if the block ends in an unconditional
              -- jump (note: calls have a fallthrough because they return)
              -> Some (MD.ParsedBlock arch)
              -> SymbolicBlock arch
symbolicBlock concAddr symAddr symInsns repr symSucc spb =
  SymbolicBlock { symbolicBlockOriginalAddress = concAddr
                , symbolicBlockSymbolicAddress = symAddr
                , symbolicBlockInstructions = symInsns
                , symbolicBlockRepr = repr
                , symbolicBlockSymbolicSuccessor = symSucc
                , symbolicBlockDiscoveryBlock = spb
                }

symbolicBlockWithoutSuccessor :: SymbolicBlock arch -> SymbolicBlock arch
symbolicBlockWithoutSuccessor sb =
  case symbolicBlockSymbolicSuccessor sb of
    Nothing -> sb
    Just _ -> sb { symbolicBlockSymbolicSuccessor = Nothing }

withSymbolicInstructions :: SymbolicBlock arch
                         -> ( forall (tp :: RCI.InstructionArchReprKind arch)
                            . ( RCI.InstructionConstraints arch tp )
                            => RCI.InstructionArchRepr arch tp -> DLN.NonEmpty (RCI.Instruction arch tp (RCR.Relocation arch)) -> a)
                         -> a
withSymbolicInstructions (SymbolicBlock _caddr _saddr insns repr _ _) k =
  k repr insns

-- | Some algorithms (such as layout) will need to assigned an address
-- to symbolic blocks and then make the blocks concrete. This type
-- bundles up a symbolic block and concrete address for that purpose.
data AddressAssignedBlock arch = AddressAssignedBlock
  { lbBlock :: SymbolicBlock arch -- ^ The block with symbolic addresses
  , lbAt    :: RCA.ConcreteAddress arch -- ^ The concrete address for this block
  , lbSize  :: Word64 -- ^ How many bytes we set aside for this block (or 0 if we aren't moving/rewriting it)
  }

-- | The result of fixing up symbolic references in an 'AddressAssignedBlock' by
-- concretizing
--
-- This is substantially like a 'ConcreteBlock', except there is no macaw block
-- corresponding to it.
data ConcretizedBlock arch =
  forall (tp :: RCI.InstructionArchReprKind arch) .
  ( RCI.InstructionConstraints arch tp ) =>
  ConcretizedBlock { concretizedBlockAddress :: RCA.ConcreteAddress arch
                   , concretizedBlockInstructions :: DLN.NonEmpty (RCI.Instruction arch tp ())
                   , concretizedBlockRepr :: RCI.InstructionArchRepr arch tp
                   }

instance Show (ConcretizedBlock arch) where
  show (ConcretizedBlock addr insns _) =
    concat [ "ConcretizedBlock "
           , show addr
           , ", "
           , show insns
           ]

instance PP.Pretty (ConcretizedBlock arch) where
  pretty (ConcretizedBlock addr insns _) =
    PP.pretty addr PP.<> PP.pretty ":" PP.<+> PP.align (PP.vsep (map PP.pretty (F.toList insns)))

concretizedBlock :: forall arch (tp :: RCI.InstructionArchReprKind arch)
                  . ( RCI.InstructionConstraints arch tp )
                 => RCA.ConcreteAddress arch
                 -> DLN.NonEmpty (RCI.Instruction arch tp ())
                 -> RCI.InstructionArchRepr arch tp
                 -> ConcretizedBlock arch
concretizedBlock = ConcretizedBlock

withConcretizedInstructions :: ConcretizedBlock arch
                            -> (forall (tp :: RCI.InstructionArchReprKind arch) . (RCI.InstructionConstraints arch tp) => RCI.InstructionArchRepr arch tp -> DLN.NonEmpty (RCI.Instruction arch tp ()) -> a)
                            -> a
withConcretizedInstructions (ConcretizedBlock _addr insns repr) k =
  k repr insns

-- | Address information for a symbolic block.
--
-- This includes a symbolic address that uniquely and symbolically
-- identifies the block.  It also includes the original concrete
-- address of the corresponding 'ConcreteBlock' for this symbolic
-- block.
data SymbolicInfo arch =
  SymbolicInfo { symbolicAddress :: RCA.SymbolicAddress arch
               , concreteAddress :: RCA.ConcreteAddress arch
               }
  deriving (Eq, Ord)

deriving instance (MC.MemWidth (MC.ArchAddrWidth arch)) => Show (SymbolicInfo arch)

instance (MC.MemWidth (MC.ArchAddrWidth arch)) => PP.Pretty (SymbolicInfo arch) where
  pretty si = PP.pretty (symbolicAddress si)

symbolicInfo :: SymbolicBlock arch -> SymbolicInfo arch
symbolicInfo sb = SymbolicInfo (symbolicBlockSymbolicAddress sb) (symbolicBlockOriginalAddress sb)

-- | Functions that work on any block that has unannotated instructions and concrete addresses
class HasConcreteAddresses b where
  -- | Compute the addresses for each instruction in a 'BasicBlock'.
  --
  -- We cannot simply make a @Map a Address@ because two identical
  -- instructions could easily occur within the same 'BasicBlock', to
  -- say nothing of the entire program.
  withInstructionAddresses :: (MC.MemWidth (MC.ArchAddrWidth arch))
                           => RI.ISA arch
                           -> b arch
                           -> (forall (tp :: RCI.InstructionArchReprKind arch)
                               . ( RCI.InstructionConstraints arch tp )
                                => RCI.InstructionArchRepr arch tp -> DLN.NonEmpty (RCI.Instruction arch tp (), RCA.ConcreteAddress arch) -> a)
                           -> a
  -- | Compute the size of a block in bytes.
  blockSize :: RI.ISA arch -> b arch -> Word64

  blockAddress :: b arch -> RCA.ConcreteAddress arch

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
                      => RI.ISA arch
                      -> (x -> RCI.Instruction arch tp ())
                      -> RCA.ConcreteAddress arch
                      -> t x
                      -> t (x, RCA.ConcreteAddress arch)
instructionAddresses' isa accessor startAddr insns =
  snd $ T.mapAccumL computeAddress startAddr insns
  where
    computeAddress addr instr =
      let absAddr = addr `RCA.addressAddOffset` fromIntegral (RI.isaInstructionSize isa (accessor instr))
      in (absAddr, (instr, addr))

-- | Compute the size of a list of instructions, in bytes.
instructionStreamSize :: (Functor f, F.Foldable f) => RI.ISA arch -> f (RCI.Instruction arch tp t) -> Word64
instructionStreamSize isa insns =
  sum $ fmap (fromIntegral . RI.isaInstructionSize isa) insns

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
                  => RI.ISA arch
                  -> MC.Memory (MC.ArchAddrWidth arch)
                  -> SymbolicBlock arch
                  -> Word64
symbolicBlockSize isa mem (SymbolicBlock origAddr _symAddr insns repr mSymSucc _) =
  fromIntegral (normalInstSizes + fallthroughInstSizes)
  where
    -- The symbolic block has tagged instructions, which have each modifiable
    -- control flow transfer instruction tagged with a 'RelocatableTarget',
    -- which must be fixed up with 'isaModifyJumpTarget'.  We start by
    -- partitioning into regular instructions that don't need to be fixed and
    -- those that do.
    normalInstSizes = sum (fmap (computeInstructionSize isa mem origAddr) insns)

    -- Determine what (if any) sequence of instructions we need to add to handle
    -- control flow fallthrough.
    fallthroughInstrSeq = fromMaybe [] $ do
      -- Guard; fail if there is no symbolic successor (and then return 0 via
      -- the fromMaybe)
      _ <- mSymSucc
      return (F.toList (RI.isaMakeRelativeJumpTo isa origAddr origAddr repr))
    fallthroughInstSizes =
      sum (fmap (fromIntegral . RI.isaInstructionSize isa) fallthroughInstrSeq)

-- | Figure out how large an instruction will be after we concretize it
--
-- Concretization includes turning symbolic addresses into concrete addresses
-- and rewriting jumps.  Note that in this iteration of renovate, every
-- instruction is passed to this function uniformly (there is no distinction
-- between jumps and non-jumps).
computeInstructionSize :: forall arch tp
                        . (MC.MemWidth (MC.ArchAddrWidth arch))
                       => RI.ISA arch
                       -> MC.Memory (MC.ArchAddrWidth arch)
                       -> RCA.ConcreteAddress arch
                       -- ^ The address allocated to the instruction; in this
                       -- case, it is simply a fake address
                       -> RCI.Instruction arch tp (RCR.Relocation arch)
                       -> Int
computeInstructionSize isa mem insnAddr taggedInstr =
  sum (fmap (fromIntegral . RI.isaInstructionSize isa) concreteInsns)
  where
    concreteInsns = RI.isaConcretizeAddresses isa mem toConcreteAddress insnAddr taggedInstr
    -- We don't have a real target yet, so we use a fake one. This does not
    -- affect the size of the instruction (since we always use the same sized
    -- jumps in generated code).
    toConcreteAddress = const insnAddr

-- | Return the 'JumpType' of the terminator instruction (if any)
--
-- Note that blocks cannot be empty
terminatorType :: (MC.MemWidth (MC.ArchAddrWidth arch)) => RI.ISA arch -> MC.Memory (MC.ArchAddrWidth arch) -> ConcreteBlock arch -> Some (RI.JumpType arch)
terminatorType isa mem b =
  withInstructionAddresses isa b $ \_repr insns ->
    let (termInsn, addr) = DLN.last insns
    in case concreteDiscoveryBlock b of
      Some pb -> RI.isaJumpType isa termInsn mem addr pb

prettyConcreteBlock :: (MC.MemWidth (MC.ArchAddrWidth arch)) => RI.ISA arch -> ConcreteBlock arch -> PP.Doc ann
prettyConcreteBlock isa (ConcreteBlock addr insns _repr _pb) =
  PP.vsep [ PP.pretty addr PP.<> PP.pretty ":"
          , PP.indent 2 (PP.vsep (map (PP.pretty . RI.isaPrettyInstruction isa) (F.toList insns)))
          ]

prettyConcretizedBlock :: (MC.MemWidth (MC.ArchAddrWidth arch)) => RI.ISA arch -> ConcretizedBlock arch -> PP.Doc ann
prettyConcretizedBlock isa (ConcretizedBlock addr insns _repr) =
  PP.vsep [ PP.pretty addr PP.<> PP.pretty ":"
          , PP.indent 2 (PP.vsep (map (PP.pretty . RI.isaPrettyInstruction isa) (F.toList insns)))
          ]
