{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
module Renovate.Diagnostic
( Diagnostic(..)
, Diagnostics(..)
)
where

import qualified Control.Monad.Catch as E
import qualified Data.Semigroup as SG
import qualified Data.Sequence as Seq
import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Typeable ( Typeable )
import           Data.Word ( Word64 )

import qualified Data.Macaw.CFG as MC

import           Renovate.Address
import           Renovate.BasicBlock ( Instruction, ConcreteBlock, basicBlockAddress, prettyBasicBlock )
import           Renovate.ISA ( ISA(..) )

-- | The types of diagnostic messages that can be generated during rewriting or
-- recovery.
data Diagnostic = forall arch t . InstructionIsNotJump (ISA arch) (Instruction arch t)
                | forall arch. (MC.MemWidth (MC.ArchAddrWidth arch)) => NoConcreteAddressForSymbolicTarget !(ConcreteAddress arch) !(SymbolicAddress arch) String
                | forall arch. (MC.MemWidth (MC.ArchAddrWidth arch)) => BlockTooSmallForRedirection (ISA arch) Word64 (ConcreteBlock arch) (ConcreteBlock arch)
                  -- ^ Indicates that the given original block was too small to hold the
                  -- required jump for a redirection (and thus cannot be redirected).
                  -- ISA, Orig block, instr block
                | forall arch . (MC.MemWidth (MC.ArchAddrWidth arch)) => OverlappingBlocks (ConcreteAddress arch) (ConcreteAddress arch) (ConcreteAddress arch)
                -- ^ Found two basic blocks that overlap.  This
                -- actually doesn't need to be an error, but it is for
                -- now...
                | forall w . MC.MemWidth w => MemoryError (MC.MemoryError w)
                | forall w . MC.MemWidth w => NoByteRegionAtAddress (MC.MemAddr w)
                deriving (Typeable)

instance Show Diagnostic where
  show = show . PD.pretty
instance E.Exception Diagnostic

instance PD.Pretty Diagnostic where
  pretty (InstructionIsNotJump isa i) =
    PD.hsep [ PD.pretty "Instruction is not a jump:"
            , PD.pretty (isaPrettyInstruction isa i)
            ]
  pretty (NoConcreteAddressForSymbolicTarget insnAddr symAddr ctx) =
    PD.hsep [ PD.pretty "No concrete address for"
            , PD.pretty symAddr
            , PD.pretty "at instruction address"
            , PD.pretty insnAddr
            , PD.parens (PD.pretty "context:" PD.<+> PD.pretty ctx)
            ]
  pretty (BlockTooSmallForRedirection isa jmpSize origBlock instrBlock) =
    PD.vsep [ PD.hsep [ PD.pretty "Basic block at"
                      , PD.pretty (basicBlockAddress origBlock)
                      , PD.pretty "is too small to hold a redirection"
                      , PD.parens (PD.pretty "requires" PD.<+> PD.pretty jmpSize PD.<+> PD.pretty "bytes")
                      ]
            , PD.pretty "Original block:"
            , PD.indent 2 (prettyBasicBlock isa origBlock)
            , PD.pretty "Instrumented block:"
            , PD.indent 2 (prettyBasicBlock isa instrBlock)
            ]
  pretty (OverlappingBlocks insnAddr nextAddr stopAddr) =
    PD.hsep [ PD.pretty "Overlapping blocks at address"
            , PD.pretty insnAddr
            , PD.parens $ PD.hsep [ PD.pretty "the next address to disassemble is"
                                  , PD.pretty nextAddr
                                  , PD.pretty "with the stop address"
                                  , PD.pretty stopAddr
                                  ]
            ]
  pretty (MemoryError memErr) = PD.pretty (show memErr)
  pretty (NoByteRegionAtAddress addr) =
    PD.hsep [ PD.pretty "No byte region at address:"
            , PD.pretty (show addr)
            ]

-- | A set of diagnostic messages emitted during a recovery or redirect -- analysis.
data Diagnostics = Diagnostics { diagnosticMessages :: !(Seq.Seq Diagnostic) }
                 deriving (Show)

instance SG.Semigroup Diagnostics where
  (<>) !d1 !d2 = Diagnostics { diagnosticMessages = diagnosticMessages d1 `mappend` diagnosticMessages d2 }

instance Monoid Diagnostics where
  mempty = Diagnostics { diagnosticMessages = Seq.empty }
  mappend = (SG.<>)

