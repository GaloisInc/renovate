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
  SymbolicBlock,
  symbolicBlock,
  symbolicBlockOriginalAddress,
  symbolicBlockSymbolicAddress,
  symbolicBlockInstructions,
  -- * Padding blocks
  PaddingBlock,
  paddingBlock,
  paddingBlockAddress,
  paddingBlockInstructions,
  -- * Fallthrough blocks
  FallthroughBlock,
  fallthroughBlock,
  fallthroughOriginalAddress,
  fallthroughSymbolicAddress,
  fallthroughInstructions,
  FallthroughInstruction(..),
  addFallthrough,
  noFallthrough,
  hasNoAddresses,
  FallthroughType(..),
  -- * Concretized blocks
  ConcretizedBlock,
  concretizedBlock,
  concretizedBlockAddress,
  concretizedBlockInstructions,

  SymbolicInfo(..),


  AddressAssignedBlock(..),
  TaggedInstruction,
  tagInstruction,
  symbolicTarget,
  projectInstruction,
  SymbolicFallthrough,
  ConcreteFallthrough,

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
concreteBlock addr insns pb = ConcreteBlock addr insns (Some pb)

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
                }

symbolicBlock :: ConcreteAddress arch
              -> SymbolicAddress arch
              -> DLN.NonEmpty (TaggedInstruction arch (InstructionAnnotation arch))
              -> SymbolicBlock arch
symbolicBlock = SymbolicBlock

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
data FallthroughType a where
  -- | A tag for instructions internal to a block that implicitly fall through
  -- to the next instruction but need no correction
  InternalInstruction :: FallthroughType a
  -- | The end of a basic block that unconditionally allows execution to fall
  -- through to the next block
  UnconditionalFallthrough :: a -> FallthroughType a
  -- | The block ends in an unconditional jump to the named target
  UnconditionalJump :: a -> FallthroughType a
  -- | The block ends in a conditional jump, and thus has two targets (in
  -- order): the target of the conditional branch and the implicit fallthrough
  -- target
  ConditionalFallthrough :: a -> a -> FallthroughType a

deriving instance Functor FallthroughType
deriving instance Foldable FallthroughType
deriving instance Traversable FallthroughType
deriving instance (Show a) => Show (FallthroughType a)

-- | A wrapper around a normal instruction that includes optional addresses for
-- all of the instruction's successors that may be relocated. The addresses can
-- be chosen to be symbolic or concrete by picking different @addr@s.
data FallthroughInstruction arch addr a = FallthroughInstruction
  { ftInstruction :: Instruction arch a
  , fallthroughType :: FallthroughType addr
  }

type SymbolicFallthrough arch = FallthroughInstruction arch (SymbolicAddress arch)
type ConcreteFallthrough arch = FallthroughInstruction arch (ConcreteAddress arch)

instance PD.Pretty (Instruction arch a) => PD.Pretty (FallthroughInstruction arch addr a) where
  pretty = PD.pretty . ftInstruction

-- | Lift a 'TaggedInstruction' to a 'FallthroughInstruction' by adding the
-- symbolic address of its fallthrough successor. If it didn't have a jump
-- target before, it's considered straightline code and the successor address
-- is ignored.
--
-- Note that this is only called on terminators, so it never produces the
-- 'InternalInstruction' case or the 'UnconditionalJump' case.  Those are
-- handled by 'noFallthrough'.
addFallthrough :: SymbolicAddress arch -> TaggedInstruction arch a -> SymbolicFallthrough arch a
addFallthrough fallthrough (Tag (i, tgt)) =
  case tgt of
    Nothing -> FallthroughInstruction i (UnconditionalFallthrough fallthrough)
    Just someTgt -> FallthroughInstruction i (ConditionalFallthrough someTgt fallthrough)

-- | Lift a 'TaggedInstruction' to a 'FallthroughInstruction' by asserting that
-- it has no fallthrough successor. If it didn't have a jump target before, it
-- shouldn't need to be rewritten. So we treat it as straightline code.
noFallthrough :: TaggedInstruction arch a -> SymbolicFallthrough arch a
noFallthrough (Tag (i, tgt)) =
  case tgt of
    Nothing -> FallthroughInstruction i InternalInstruction
    Just someTgt -> FallthroughInstruction i (UnconditionalJump someTgt)

-- | Return 'True' if the 'FallthroughInstruction' has no target or fallthrough addresses.
hasNoAddresses :: FallthroughInstruction arch addr a -> Bool
hasNoAddresses fi =
  case fallthroughType fi of
    InternalInstruction -> True
    UnconditionalFallthrough {} -> False
    UnconditionalJump {} -> False
    ConditionalFallthrough {} -> False

-- | The type of 'BasicBlock's that have up to two symbolic address targets for jumps.
--
-- The extra annotations live on the terminator instruction.
--
-- Unconditional jumps are annotated with symbolic address targets, and
-- conditional jumps are annotated with two symbolic addresses for their target
-- and fallthrough behavior.
data FallthroughBlock arch =
  FallthroughBlock { fallthroughOriginalAddress :: ConcreteAddress arch
                   , fallthroughSymbolicAddress :: SymbolicAddress arch
                   , fallthroughInstructions :: DLN.NonEmpty (FallthroughInstruction arch (SymbolicAddress arch) (InstructionAnnotation arch))
                   }

fallthroughBlock :: ConcreteAddress arch
                 -> SymbolicAddress arch
                 -> DLN.NonEmpty (FallthroughInstruction arch (SymbolicAddress arch) (InstructionAnnotation arch))
                 -> FallthroughBlock arch
fallthroughBlock = FallthroughBlock

-- | Some algorithms (such as layout) will need to assigned an address
-- to symbolic blocks and then make the blocks concrete. This type
-- bundles up a symbolic block and concrete address for that purpose.
data AddressAssignedBlock arch = AddressAssignedBlock
  { lbBlock :: FallthroughBlock arch -- ^ The block with symbolic addresses
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
