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
import           Data.Typeable ( Typeable )
import           Data.Word ( Word64 )

import qualified Data.Macaw.CFG as MC

import           Renovate.Address

-- | The types of diagnostic messages that can be generated during rewriting or
-- recovery.
data Diagnostic = forall arch. (MC.MemWidth (MC.ArchAddrWidth arch)) => NoSymbolicAddressForTarget String (ConcreteAddress arch) String
                  -- ^ A concrete address was expected to have a
                  -- symbolic equivalent, but it did not.  The string
                  -- describes the context in which the address was
                  -- missing.
                | InstructionIsNotJump String
                | forall arch. (MC.MemWidth (MC.ArchAddrWidth arch)) => NoConcreteAddressForSymbolicTarget !(SymbolicAddress arch) String
                | forall arch. (MC.MemWidth (MC.ArchAddrWidth arch)) => BlockTooSmallForRedirection
                    !Word64{- block size-} !Word64{- jump instr size -}
                    (ConcreteAddress arch){- address of block-} !String{- show of block-}
                  -- ^ The 'BasicBlock' at 'Address' is too small to
                  -- be redirected, because its size is larger than
                  -- the size of a jump instruction.
                | forall arch . (MC.MemWidth (MC.ArchAddrWidth arch)) => OverlappingBlocks (ConcreteAddress arch)
                -- ^ Found two basic blocks that overlap.  This
                -- actually doesn't need to be an error, but it is for
                -- now...
                -- | UnalignedBlockSplit Address
                -- | FailedToFindSplit Address
                -- | MultipleBlocksInSplit Address
                -- | InvalidComputedAddress Address Address
                | DecodingError E.SomeException
                | forall w . MC.MemWidth w => MemoryError (MC.MemoryError w)
                | forall w . MC.MemWidth w => NoByteRegionAtAddress (MC.MemAddr w)
                deriving (Typeable)

deriving instance Show Diagnostic
instance E.Exception Diagnostic

-- | A set of diagnostic messages emitted during a recovery or redirect -- analysis.
data Diagnostics = Diagnostics { diagnosticMessages :: !(Seq.Seq Diagnostic) }
                 deriving (Show)

instance SG.Semigroup Diagnostics where
  (<>) !d1 !d2 = Diagnostics { diagnosticMessages = diagnosticMessages d1 `mappend` diagnosticMessages d2 }

instance Monoid Diagnostics where
  mempty = Diagnostics { diagnosticMessages = Seq.empty }
  mappend = (SG.<>)

