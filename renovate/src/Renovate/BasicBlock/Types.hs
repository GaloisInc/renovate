{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Renovate.BasicBlock.Types (
  Instruction,
  InstructionAnnotation,
  RegisterType,
  -- * Concrete blocks
  ConcreteBlock,
  concreteBlock,
  concreteBlockAddress,
  concreteBlockInstructions,
  concreteDiscoveryBlock,
  -- * Symbolic blocks
  SymbolicBlock(..),
  symbolicBlock,
  symbolicBlockWithoutSuccessor,
  SymbolicInfo(..),
  -- * Padding blocks
  PaddingBlock,
  paddingBlock,
  paddingBlockAddress,
  paddingBlockInstructions,
  -- * Concretized blocks
  ConcretizedBlock,
  concretizedBlock,
  concretizedBlockAddress,
  concretizedBlockInstructions,
  AddressAssignedBlock(..),
  TaggedInstruction,
  tagInstruction,
  symbolicTarget,
  projectInstruction,

  ToGenericInstruction(..),
  -- * Constraints
  InstructionConstraints
  ) where

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Typeable ( Typeable )
import           Data.Word

import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MD

import qualified SemMC.Architecture as SA

import           Renovate.Address

-- | The type of instructions for an architecture
--
-- Instructions are parameterized by an annotation type that is usually either
-- '()' or @'InstructionAnnotation' arch@
type family Instruction arch :: * -> *

-- | The type of annotations used to carry relocation information while rewriting
type family InstructionAnnotation arch :: *

-- | The type of register values for the architecture
type family RegisterType arch :: *

-- | Concrete renovate instructions of the type @'Instruction' arch ()@ are in
-- several cases equivalent to semmc instructions of type
-- @'SemMC.Architecture.Instruction' arch@. This property is not true for X86
-- instructions, but is true for PowerPC.
class ToGenericInstruction arch
  where
    toGenericInstruction   :: Instruction arch a  -> SA.Instruction arch
    fromGenericInstruction :: SA.Instruction arch -> Instruction arch  ()

-- | Constraints common to all instructions.
--
-- All 'Instruction' instances must be 'Show'able and 'Typeable'.  They are
-- combined for convenience and to reduce noise in type signatures, since those
-- constraints are not very interesting.
type InstructionConstraints arch =
  ( PD.Pretty (Instruction arch ())
  , PD.Pretty (Instruction arch (InstructionAnnotation arch))
  , Show (Instruction arch (InstructionAnnotation arch))
  , Show (Instruction arch ())
  , Eq (Instruction arch ())
  , Ord (RegisterType arch)
  , Typeable arch
  , Typeable (Instruction arch)
  , Typeable (InstructionAnnotation arch)
  , MC.MemWidth (MC.ArchAddrWidth arch)
  )

-- | The 'ConcreteBlock' is a basic block (address + non-empty list of
-- instructions) with an associated macaw 'MD.ParsedBlock'.
--
-- These are the blocks produced by the code discovery process (see
-- Recovery.hs).  As they always come from macaw blocks, we have a macaw block
-- to associate with them on a one-to-one basis.
data ConcreteBlock arch =
  ConcreteBlock { concreteBlockAddress :: ConcreteAddress arch
                , concreteBlockInstructions :: DLN.NonEmpty (Instruction arch ())
                , concreteDiscoveryBlock :: Some (MD.ParsedBlock arch)
                -- ^ The macaw block that generated this 'ConcreteBlock'
                }

instance (MC.MemWidth (MC.ArchAddrWidth arch), Show (Instruction arch ())) => Show (ConcreteBlock arch) where
  show cb = concat [ "ConcreteBlock "
                   , show (concreteBlockAddress cb)
                   , " "
                   , show (concreteBlockInstructions cb)
                   ]

instance (MC.MemWidth (MC.ArchAddrWidth arch), PD.Pretty (Instruction arch ())) => PD.Pretty (ConcreteBlock arch) where
  pretty (ConcreteBlock addr insns _) =
    PD.pretty addr PD.<> ":" PD.<+> PD.align (PD.vsep (map PD.pretty (F.toList insns)))


concreteBlock :: ConcreteAddress arch
              -> DLN.NonEmpty (Instruction arch ())
              -> MD.ParsedBlock arch ids
              -> ConcreteBlock arch
concreteBlock addr insns pb =
  ConcreteBlock addr insns (Some pb)

-- | This is a basic block that contains padding bytes at a given address.
--
-- These have limited use (mostly in the layout code when we need to place
-- padding between other blocks.  Note that this is not just a number of bytes,
-- as some users want to put meaningful data in padding.
data PaddingBlock arch =
  PaddingBlock { paddingBlockAddress :: ConcreteAddress arch
               , paddingBlockInstructions :: DLN.NonEmpty (Instruction arch ())
               }

paddingBlock :: ConcreteAddress arch
                 -> DLN.NonEmpty (Instruction arch ())
                 -> PaddingBlock arch
paddingBlock = PaddingBlock

-- | A wrapper around a normal instruction that includes an optional
-- 'SymbolicAddress'.
--
-- When we start relocating code, we need to track the targets of
-- jumps *symbolically* (i.e., not based on concrete address target).
-- That allows us to stitch the jumps in the instrumented blocks
-- together.  This wrapper holds the symbolic address.
--
-- Note: this representation isn't good for instructions with more
-- than one possible target (if there is such a thing).  If that
-- becomes an issue, the annotation will need to sink into the operand
-- annotations, and we'll need a helper to collect those.
newtype TaggedInstruction arch a = Tag { unTag :: (Instruction arch a, Maybe (SymbolicAddress arch)) }

instance PD.Pretty (Instruction arch a) => PD.Pretty (TaggedInstruction arch a) where
  pretty (Tag (i, _)) = PD.pretty i

-- | Annotate an instruction with a symbolic target.
--
-- We use this if the instruction is a jump and we will need to
-- relocate it, so we need to know the symbolic block it is jumping
-- to.
tagInstruction :: Maybe (SymbolicAddress arch) -> Instruction arch a -> TaggedInstruction arch a
tagInstruction ma i = Tag (i, ma)

-- | If the 'TaggedInstruction' has a 'SymbolicAddress' as a target,
-- return it.
symbolicTarget :: TaggedInstruction arch a -> Maybe (SymbolicAddress arch)
symbolicTarget = snd . unTag

-- | Remove the tag from an instruction
projectInstruction :: TaggedInstruction arch a -> Instruction arch a
projectInstruction = fst . unTag

-- | The type of 'BasicBlock's that only have symbolic addresses.
-- Their jumps are annotated with symbolic address targets as well,
-- which refer to other 'SymbolicBlock's.
data SymbolicBlock arch =
  SymbolicBlock { symbolicBlockOriginalAddress :: ConcreteAddress arch
                , symbolicBlockSymbolicAddress :: SymbolicAddress arch
                , symbolicBlockInstructions :: DLN.NonEmpty (TaggedInstruction arch (InstructionAnnotation arch))
                , symbolicBlockSymbolicSuccessor :: Maybe (SymbolicAddress arch)
                }

symbolicBlock :: ConcreteBlock arch
              -- ^ The 'ConcreteBlock' this 'SymbolicBlock' was lifted from
              -> SymbolicAddress arch
              -- ^ The symbolic address assigned to this block
              -> DLN.NonEmpty (TaggedInstruction arch (InstructionAnnotation arch))
              -- ^ Instructions with symbolic address annotations
              -> Maybe (SymbolicAddress arch)
              -- ^ The symbolic address of the successor, if any
              --
              -- This could be Nothing if the block ends in an unconditional
              -- jump (note: calls have a fallthrough because they return)
              -> SymbolicBlock arch
symbolicBlock cb symAddr symInsns symSucc =
  SymbolicBlock { symbolicBlockOriginalAddress = concreteBlockAddress cb
                , symbolicBlockSymbolicAddress = symAddr
                , symbolicBlockInstructions = symInsns
                , symbolicBlockSymbolicSuccessor = symSucc
                }

symbolicBlockWithoutSuccessor :: SymbolicBlock arch -> SymbolicBlock arch
symbolicBlockWithoutSuccessor sb =
  case symbolicBlockSymbolicSuccessor sb of
    Nothing -> sb
    Just _ -> sb { symbolicBlockSymbolicSuccessor = Nothing }

-- | Some algorithms (such as layout) will need to assigned an address
-- to symbolic blocks and then make the blocks concrete. This type
-- bundles up a symbolic block and concrete address for that purpose.
data AddressAssignedBlock arch = AddressAssignedBlock
  { lbBlock :: SymbolicBlock arch -- ^ The block with symbolic addresses
  , lbAt    :: ConcreteAddress arch -- ^ The concrete address for this block
  , lbSize  :: Word64 -- ^ How many bytes we set aside for this block (or 0 if we aren't moving/rewriting it)
  }

-- | The result of fixing up symbolic references in an 'AddressAssignedBlock' by
-- concretizing
--
-- This is substantially like a 'ConcreteBlock', except there is no macaw block
-- corresponding to it.
data ConcretizedBlock arch =
  ConcretizedBlock { concretizedBlockAddress :: ConcreteAddress arch
                   , concretizedBlockInstructions :: DLN.NonEmpty (Instruction arch ())
                   }

deriving instance (Show (Instruction arch ()), MC.MemWidth (MC.ArchAddrWidth arch)) => Show (ConcretizedBlock arch)

instance (MC.MemWidth (MC.ArchAddrWidth arch), PD.Pretty (Instruction arch ())) => PD.Pretty (ConcretizedBlock arch) where
  pretty (ConcretizedBlock addr insns) =
    PD.pretty addr PD.<> ":" PD.<+> PD.align (PD.vsep (map PD.pretty (F.toList insns)))

concretizedBlock :: ConcreteAddress arch
                 -> DLN.NonEmpty (Instruction arch ())
                 -> ConcretizedBlock arch
concretizedBlock = ConcretizedBlock











-- | Address information for a symbolic block.
--
-- This includes a symbolic address that uniquely and symbolically
-- identifies the block.  It also includes the original concrete
-- address of the corresponding 'ConcreteBlock' for this symbolic
-- block.
data SymbolicInfo arch = SymbolicInfo { symbolicAddress :: SymbolicAddress arch
                                      , concreteAddress :: ConcreteAddress arch
                                      }
                    deriving (Eq, Ord)

deriving instance (MC.MemWidth (MC.ArchAddrWidth arch)) => Show (SymbolicInfo arch)

instance (MC.MemWidth (MC.ArchAddrWidth arch)) => PD.Pretty (SymbolicInfo arch) where
  pretty si = PD.pretty (symbolicAddress si)
