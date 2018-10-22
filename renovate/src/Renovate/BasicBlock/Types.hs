{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Renovate.BasicBlock.Types (
  BasicBlock(..),
  Instruction,
  InstructionAnnotation,
  RegisterType,
  ConcreteBlock,
  SymbolicBlock,
  SymbolicInfo(..),
  AddressAssignedBlock(..),
  TaggedInstruction,
  tagInstruction,
  hasNoSymbolicTarget,
  symbolicTarget,
  projectInstruction,
  ToGenericInstruction(..),
  -- * Constraints
  InstructionConstraints
  ) where

import           Data.Maybe ( isNothing )
import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Typeable ( Typeable )

import qualified Data.Macaw.CFG as MC

import qualified SemMC.Architecture as SA

import           Renovate.Address

-- | A basic block is a list of instructions (whose sizes and specific
-- information we can find from the ISA).  Basic blocks also have a
-- starting address, which is polymorphic.
--
-- The intent is that basic blocks can have either concrete or
-- symbolic addresses (instantiated as 'ConcreteBlock' and
-- 'SymbolicBlock', respectively)
data BasicBlock addr (i :: * -> *) a =
  BasicBlock { basicBlockInstructions :: [i a]
             , basicBlockAddress :: addr
             }
  deriving (Eq, Show)

instance (PD.Pretty addr, PD.Pretty (i a)) => PD.Pretty (BasicBlock addr i a) where
  pretty (BasicBlock insns addr) =
    PD.pretty addr PD.<> ":" PD.<+> PD.align (PD.vsep (map PD.pretty insns))

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
  , Typeable (Instruction arch (InstructionAnnotation arch))
  , Typeable (Instruction arch)
  , Typeable (InstructionAnnotation arch)
  , Typeable (Instruction arch ())
  , MC.MemWidth (MC.ArchAddrWidth arch)
  )

-- | The type of concrete 'BasicBlock's that have been assigned real
-- addresses.
--
-- Note that we use 'MC.MemWord' for the address of blocks rather than
-- 'MC.SegmentedAddr', as we need to create blocks that haven't yet
-- been assigned to a segment, so we can't actually give them a
-- 'MC.SegmentedAddr' (there is no segment for them to refer to).
type ConcreteBlock arch = BasicBlock (ConcreteAddress arch) (Instruction arch) ()

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

-- | Return 'True' if the 'TaggedInstruction' has no symbolic target.
hasNoSymbolicTarget :: TaggedInstruction arch a -> Bool
hasNoSymbolicTarget = isNothing . symbolicTarget

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
type SymbolicBlock arch = BasicBlock (SymbolicInfo arch) (TaggedInstruction arch) (InstructionAnnotation arch)

-- | Some algorithms (such as layout) will need to assigned an address
-- to symbolic blocks and then make the blocks concrete. This type
-- bundles up a symbolic block and concrete address for that purpose.
data AddressAssignedBlock arch = AddressAssignedBlock
  { lbBlock :: SymbolicBlock arch   -- ^ The symbolic block
  , lbAt    :: ConcreteAddress arch -- ^ The concrete address for this block
  }

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

