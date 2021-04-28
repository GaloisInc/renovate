{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module Renovate.BasicBlock.Types (
  InstructionArchRepr,
  InstructionArchReprKind,
  SomeInstructionArchRepr(..),
  Instruction,
  ArchitectureRelocation,
  Relocation(..),
  RegisterType,
  -- * Concrete blocks
  ConcreteBlock(..),
  concreteBlock,
  withConcreteInstructions,
  -- * Symbolic blocks
  SymbolicBlock(..),
  symbolicBlock,
  symbolicBlockWithoutSuccessor,
  withSymbolicInstructions,
  -- * Padding blocks
  PaddingBlock,
  paddingBlock,
  paddingBlockAddress,
  withPaddingInstructions,
  -- * Concretized blocks
  ConcretizedBlock(..),
  concretizedBlock,
  withConcretizedInstructions,
  SymbolicInfo(..),
  symbolicInfo,
  AddressAssignedBlock(..),
  ToGenericInstruction(..),
  -- * Constraints
  ArchConstraints,
  InstructionConstraints
  ) where

import qualified Data.Foldable as F
import           Data.Kind ( Type )
import qualified Data.List.NonEmpty as DLN
import           Data.Parameterized.Classes
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Word

import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MD

import qualified SemMC.Architecture as SA

import           Renovate.Address

type family InstructionArchReprKind arch :: k

-- | A value-level representative of the type of instructions in a basic block
--
-- The intent of this is to support architectures with multiple instruction sets
-- (e.g., AArch32 with ARM and Thumb).  This lets us enforce that each basic
-- block has instructions from only a single architecture.
type family InstructionArchRepr arch :: InstructionArchReprKind arch -> Type

type ArchConstraints arch (tp :: InstructionArchReprKind arch) =
  ( MC.MemWidth (MC.ArchAddrWidth arch)
  , Show (Instruction arch tp ())
  , PD.Pretty (Instruction arch tp ())
  , TestEquality (InstructionArchRepr arch)
  , OrdF (InstructionArchRepr arch)
  , Ord (RegisterType arch tp)
  , Eq (RegisterType arch tp)
  , OrdF (RegisterType arch)
  , InstructionConstraints arch
  )

data SomeInstructionArchRepr arch where
  SomeInstructionArchRepr :: ( ArchConstraints arch tp )
                          => InstructionArchRepr arch tp
                          -> SomeInstructionArchRepr arch

-- | The type of instructions for an architecture
--
-- Instructions are parameterized by an annotation type that is usually either
-- '()' or @'InstructionAnnotation' arch@
type family Instruction arch :: InstructionArchReprKind arch -> Type -> Type

-- | An architecture-specific relocation extension type
--
-- If this is not needed, instantiate it as 'Void'
type family ArchitectureRelocation arch :: Type

-- | Representations of relocations that must be resolved during instruction concretization
--
-- Each operand is annotated with a relocation that contains enough information
-- to be resolved during the concretization phase of rewriting. These
-- relocations are analogous to those in object files, and represent symbolic
-- addresses that must be concretized.
data Relocation arch where
  -- | A reference to an absolute address by means of a PC-relative offset that
  -- needs to be re-computed when the PC of the instruction changes
  --
  -- These are largely used for referencing existing data values in a
  -- position-independent way, as data is never assigned a symbolic address (as
  -- moving existing data is too dangerous in general)
  PCRelativeRelocation :: ConcreteAddress arch -> Relocation arch
  -- | A reference to a symbolic location that should be referenced (probably by
  -- a PC-relative offset) once both the containing instruction and target have
  -- been assigned addresses.
  --
  -- These are assigned to injected code, injected data, and code that may move
  -- (e.g., jump targets)
  SymbolicRelocation :: SymbolicAddress arch -> Relocation arch
  -- | An architecture-specific relocation type
  --
  -- If this is not needed, instantiate the 'ArchitectureRelocation' type as 'Void'
  ArchRelocation :: ArchitectureRelocation arch -> Relocation arch
  -- | For operands that do not require relocations
  NoRelocation :: Relocation arch

deriving instance (Show (ArchitectureRelocation arch), MC.MemWidth (MC.ArchAddrWidth arch)) => Show (Relocation arch)

-- | The type of register values for the architecture
type family RegisterType arch :: InstructionArchReprKind arch -> Type


-- | Concrete renovate instructions of the type @'Instruction' arch ()@ are in
-- several cases equivalent to semmc instructions of type
-- @'SemMC.Architecture.Instruction' arch@. This property is not true for X86
-- instructions, but is true for PowerPC.
class ToGenericInstruction arch
  where
    toGenericInstruction   :: forall (tp :: InstructionArchReprKind arch) a . Instruction arch tp a -> SA.Instruction arch
    fromGenericInstruction :: forall (tp :: InstructionArchReprKind arch) . InstructionArchRepr arch tp -> SA.Instruction arch -> Instruction arch tp ()

-- | Constraints common to all instructions.
--
-- All 'Instruction' instances must be 'Show'able and 'Typeable'.  They are
-- combined for convenience and to reduce noise in type signatures, since those
-- constraints are not very interesting.
type InstructionConstraints arch =
  ( PD.Pretty (Instruction arch (InstructionArchReprKind arch) ())
  , Show (Instruction arch (InstructionArchReprKind arch) ())
  , Eq (Instruction arch (InstructionArchReprKind arch) ())
  , Ord (RegisterType arch (InstructionArchReprKind arch))
  , MC.MemWidth (MC.ArchAddrWidth arch)
  )

-- | The 'ConcreteBlock' is a basic block (address + non-empty list of
-- instructions) with an associated macaw 'MD.ParsedBlock'.
--
-- These are the blocks produced by the code discovery process (see
-- Recovery.hs).  As they always come from macaw blocks, we have a macaw block
-- to associate with them on a one-to-one basis.
data ConcreteBlock arch =
  forall (tp :: InstructionArchReprKind arch) .
  ( ArchConstraints arch tp ) =>
  ConcreteBlock { concreteBlockAddress :: ConcreteAddress arch
                , concreteBlockInstructions :: DLN.NonEmpty (Instruction arch tp ())
                , concreteBlockRepr :: InstructionArchRepr arch tp
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

instance PD.Pretty (ConcreteBlock arch) where
  pretty (ConcreteBlock addr insns _ _) =
    PD.pretty addr PD.<> ":" PD.<+> PD.align (PD.vsep (map PD.pretty (F.toList insns)))


concreteBlock :: forall arch (tp :: InstructionArchReprKind arch) ids
               . ( ArchConstraints arch tp )
              => ConcreteAddress arch
              -> DLN.NonEmpty (Instruction arch tp ())
              -> InstructionArchRepr arch tp
              -> MD.ParsedBlock arch ids
              -> ConcreteBlock arch
concreteBlock addr insns repr pb =
  ConcreteBlock addr insns repr (Some pb)

withConcreteInstructions :: ConcreteBlock arch
                         -> ( forall (tp :: InstructionArchReprKind arch)
                            . ( ArchConstraints arch tp )
                           => InstructionArchRepr arch tp -> DLN.NonEmpty (Instruction arch tp ()) -> a)
                         -> a
withConcreteInstructions (ConcreteBlock _addr insns repr _) k =
  k repr insns

-- | This is a basic block that contains padding bytes at a given address.
--
-- These have limited use (mostly in the layout code when we need to place
-- padding between other blocks.  Note that this is not just a number of bytes,
-- as some users want to put meaningful data in padding.
data PaddingBlock arch =
  forall (tp :: InstructionArchReprKind arch) .
  ( ArchConstraints arch tp ) =>
  PaddingBlock { paddingBlockAddress :: ConcreteAddress arch
               , _paddingBlockInstructions :: DLN.NonEmpty (Instruction arch tp ())
               , _paddingBlockRepr :: InstructionArchRepr arch tp
               }

paddingBlock :: forall arch (tp :: InstructionArchReprKind arch)
              . ( ArchConstraints arch tp )
             => ConcreteAddress arch
             -> DLN.NonEmpty (Instruction arch tp ())
             -> InstructionArchRepr arch tp
             -> PaddingBlock arch
paddingBlock = PaddingBlock

withPaddingInstructions :: PaddingBlock arch
                        -> ( forall (tp :: InstructionArchReprKind arch)
                           . ( ArchConstraints arch tp )
                           => InstructionArchRepr arch tp -> DLN.NonEmpty (Instruction arch tp ()) -> a)
                        -> a
withPaddingInstructions (PaddingBlock _ insns repr) k =
  k repr insns

-- | The type of 'BasicBlock's that only have symbolic addresses.
-- Their jumps are annotated with symbolic address targets as well,
-- which refer to other 'SymbolicBlock's.
data SymbolicBlock arch =
  forall (tp :: InstructionArchReprKind arch) .
  ( ArchConstraints arch tp ) =>
  SymbolicBlock { symbolicBlockOriginalAddress :: ConcreteAddress arch
                , symbolicBlockSymbolicAddress :: SymbolicAddress arch
                , symbolicBlockInstructions :: DLN.NonEmpty (Instruction arch tp (Relocation arch))
                , symbolicBlockRepr :: InstructionArchRepr arch tp
                , symbolicBlockSymbolicSuccessor :: Maybe (SymbolicAddress arch)
                , symbolicBlockDiscoveryBlock :: Some (MD.ParsedBlock arch)
                }

symbolicBlock :: forall arch (tp :: InstructionArchReprKind arch)
               . ( ArchConstraints arch tp )
              => ConcreteAddress arch
              -- ^ The 'ConcreteBlock' this 'SymbolicBlock' was lifted from
              -> SymbolicAddress arch
              -- ^ The symbolic address assigned to this block
              -> DLN.NonEmpty (Instruction arch tp (Relocation arch))
              -- ^ Instructions with symbolic address annotations
              -> InstructionArchRepr arch tp
              -> Maybe (SymbolicAddress arch)
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
                         -> ( forall (tp :: InstructionArchReprKind arch)
                            . ( ArchConstraints arch tp )
                            => InstructionArchRepr arch tp -> DLN.NonEmpty (Instruction arch tp (Relocation arch)) -> a)
                         -> a
withSymbolicInstructions (SymbolicBlock _caddr _saddr insns repr _ _) k =
  k repr insns

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
  forall (tp :: InstructionArchReprKind arch) .
  ( ArchConstraints arch tp ) =>
  ConcretizedBlock { concretizedBlockAddress :: ConcreteAddress arch
                   , concretizedBlockInstructions :: DLN.NonEmpty (Instruction arch tp ())
                   , concretizedBlockRepr :: InstructionArchRepr arch tp
                   }

instance Show (ConcretizedBlock arch) where
  show (ConcretizedBlock addr insns _) =
    concat [ "ConcretizedBlock "
           , show addr
           , ", "
           , show insns
           ]

instance PD.Pretty (ConcretizedBlock arch) where
  pretty (ConcretizedBlock addr insns _) =
    PD.pretty addr PD.<> ":" PD.<+> PD.align (PD.vsep (map PD.pretty (F.toList insns)))

concretizedBlock :: forall arch (tp :: InstructionArchReprKind arch)
                  . ( ArchConstraints arch tp )
                 => ConcreteAddress arch
                 -> DLN.NonEmpty (Instruction arch tp ())
                 -> InstructionArchRepr arch tp
                 -> ConcretizedBlock arch
concretizedBlock = ConcretizedBlock

withConcretizedInstructions :: ConcretizedBlock arch
                            -> (forall (tp :: InstructionArchReprKind arch) . (ArchConstraints arch tp) => InstructionArchRepr arch tp -> DLN.NonEmpty (Instruction arch tp ()) -> a)
                            -> a
withConcretizedInstructions (ConcretizedBlock _addr insns repr) k =
  k repr insns









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

symbolicInfo :: SymbolicBlock arch -> SymbolicInfo arch
symbolicInfo sb = SymbolicInfo (symbolicBlockSymbolicAddress sb) (symbolicBlockOriginalAddress sb)
