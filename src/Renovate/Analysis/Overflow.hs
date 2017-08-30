{-# LANGUAGE RankNTypes #-}
-- | An interface for finding possible integer overflows
--
-- The analysis is parameterized by instruction type, and must be
-- instantiated for each ISA.
module SFE.Analysis.Overflow (
  OverflowAnalysis(..)
  ) where

-- | The interface required to perform an overflow analysis.  @i@ is
-- the instruction type, while @r@ is the register type.
--
-- The methods in this interface will provide information about
-- particular operands.
data OverflowAnalysis i r =
  OverflowAnalysis { oaTaintsRegister :: forall t . i t -> Maybe r
                   -- ^ Taint a register through arithmetic operation.
                   , oaPropagatesTaint :: forall t . i t -> [(r, Maybe r)]
                   -- ^ Instruction @i@ causes values to flow from the
                   -- first r to the second.  If the second is
                   -- Nothing, taint is cleared from the first
                   -- register.
                   , oaTaintSinks :: forall t . i t -> [r]
                   -- ^ Returns the set of taint sinks in the instruction.
                   -- For example, it could return an array index register.
                   }
