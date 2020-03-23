{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
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
  ToGenericInstruction(..),
  -- * Constraints
  InstructionConstraints
  ) where

import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Typeable ( Typeable )
import           Data.Word

import qualified Data.Parameterized.Some as PU
import qualified Data.Macaw.Discovery as MC


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
data BasicBlock (addr :: *) (instr :: * -> *) retTy =
  BasicBlock { basicBlockInstructions :: [instr retTy]
             -- ^ The block's instructions
             , basicBlockAddress :: addr
             -- ^ The block's starting address
             , basicParsedBlock :: Maybe (PU.Some (MC.ParsedBlock (ArchOf addr)))
             -- ^ The corresponding Macaw ParsedBlock (if applicable)
             }

instance (Show (i a), Show addr) => Show (BasicBlock addr i a) where
  show (BasicBlock is1 as1 _) = "(BasicBlock "++(show is1)++(show as1)++")"

instance (PD.Pretty addr, PD.Pretty (i a)) => PD.Pretty (BasicBlock addr i a) where
  pretty (BasicBlock insns addr _) =
    PD.pretty addr PD.<> ":" PD.<+> PD.align (PD.vsep (map PD.pretty insns))

type instance ArchOf (BasicBlock addr instr retTy) = (ArchOf addr)


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
type SymbolicBlock arch =
  BasicBlock (SymbolicInfo arch) (TaggedInstruction arch) (InstructionAnnotation arch)

-- | When rewriting code after relocating it, we need to track each
-- instruction's possible successors. In most cases, for straight-line code,
-- there is just one successor (the immediately next instruction in memory),
-- and as an optimization we keep straight-line code blocks together during
-- relocation. So we distinguish these cases:
--
-- 1. Straight-line code, which always implicitly falls through to the "next"
--    instruction. In most cases, we ensure that the next instruction is the
--    same in both the original and rewritten code, so these instructions need
--    not be rewritten and need not store a successor at all.
-- 2. Straight-line code that ends a basic block (because another block targets
--    the next instruction). These instructions fall through to another block,
--    which may have been relocated, so we must track the location of their
--    fallthrough successor.
-- 2. Unconditional jumps, which definitely redirect control flow. These need
--    one address for their target.
-- 3. Conditional jumps, which may either redirect control flow or implicitly
--    fall through to the "next" instruction. Because we may relocate both the
--    target of the jump and the implicit fall-through instructions, we need
--    two addresses.
--
-- We use the parameter to choose between symbolic and concrete
-- addresses for the successors.
data FallthroughTag a = FallthroughTag
  { ftTarget :: Maybe a
  , ftFallthrough :: Maybe a
  } deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A wrapper around a normal instruction that includes optional addresses for
-- all of the instruction's successors that may be relocated. The addresses can
-- be chosen to be symbolic or concrete by picking different @addr@s.
data FallthroughInstruction (addr :: *) (a :: *) =
  FallthroughInstruction
  { ftInstruction :: Instruction (ArchOf addr) a
  , ftTag :: FallthroughTag addr
  }

type SymbolicFallthrough (arch :: *) = FallthroughInstruction (SymbolicAddress arch)
type ConcreteFallthrough (arch :: *) = FallthroughInstruction (ConcreteAddress arch)

instance PD.Pretty (Instruction (ArchOf addr) a) => PD.Pretty (FallthroughInstruction addr a) where
  pretty = PD.pretty . ftInstruction

-- | Lift a 'TaggedInstruction' to a 'FallthroughInstruction' by adding the
-- symbolic address of its fallthrough successor. If it didn't have a jump
-- target before, it's considered straightline code and the successor address
-- is ignored.
addFallthrough :: SymbolicAddress arch -> TaggedInstruction arch a -> SymbolicFallthrough arch a
addFallthrough fallthrough (Tag (i, tgt)) = FallthroughInstruction i (FallthroughTag tgt (Just fallthrough))

-- | Lift a 'TaggedInstruction' to a 'FallthroughInstruction' by asserting that
-- it has no fallthrough successor. If it didn't have a jump target before, it
-- shouldn't need to be rewritten. So we treat it as straightline code.
noFallthrough :: TaggedInstruction arch a -> SymbolicFallthrough arch a
noFallthrough (Tag (i, tgt)) = FallthroughInstruction i (FallthroughTag tgt Nothing)

-- | Return 'True' if the 'FallthroughInstruction' has no target or fallthrough addresses.
hasNoAddresses :: FallthroughInstruction arch a -> Bool
hasNoAddresses fi = case ftTag fi of
  FallthroughTag Nothing Nothing -> True
  _ -> False

-- | The type of 'BasicBlock's that have up to two symbolic addresses.
-- Unconditional jumps are annotated with symbolic address targets, and
-- conditional jumps are annotated with two symbolic addresses for their target
-- and fallthrough behavior.
type FallthroughBlock arch =
  BasicBlock (SymbolicInfo arch) (SymbolicFallthrough arch) (InstructionAnnotation arch)

-- | Some algorithms (such as layout) will need to assigned an address
-- to symbolic blocks and then make the blocks concrete. This type
-- bundles up a symbolic block and concrete address for that purpose.
data AddressAssignedBlock arch = AddressAssignedBlock
  { lbBlock :: FallthroughBlock arch -- ^ The block with symbolic addresses
  , lbAt    :: ConcreteAddress arch -- ^ The concrete address for this block
  , lbSize  :: Word64 -- ^ How many bytes we set aside for this block (or 0 if we aren't moving/rewriting it)
  }

type instance ArchOf (AddressAssignedBlock arch) = arch

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

type instance (ArchOf (SymbolicInfo arch)) = arch
