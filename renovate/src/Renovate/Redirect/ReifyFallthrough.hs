{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Renovate.Redirect.ReifyFallthrough (
  reifyFallthrough
  ) where

import qualified Data.List.NonEmpty as DLN
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Memory as MM
import           Data.Parameterized.Some ( Some(..) )

import qualified Renovate.Core.Address as RA
import qualified Renovate.Core.BasicBlock as RB
import qualified Renovate.ISA as RI

-- | Compute fallthrough successors of basic blocks
--
-- The reason we don't do this with just the 'RB.ConcreteBlock' is that there is
-- some non-trivial logic for deciding if a block has a fallthrough successor,
-- and it is a bit cleaner to separate it out.
reifyFallthrough :: (MM.MemWidth (MC.ArchAddrWidth arch))
                 => RI.ISA arch
                 -> MM.Memory (MC.ArchAddrWidth arch)
                 -> RB.ConcreteBlock arch
                 -> Maybe (RA.ConcreteAddress arch)
reifyFallthrough isa mem cb =
  RB.withInstructionAddresses isa cb $ \_repr insns -> do
    case (RB.concreteDiscoveryBlock cb, DLN.last insns) of
      (Some pb, (lastInsn, lastInsnAddr))
        | isUnconditionalJT (RI.isaJumpType isa lastInsn mem lastInsnAddr) -> Nothing
        | otherwise -> do
            let sz = MD.blockSize pb
            let succAddr = RB.concreteBlockAddress cb `RA.addressAddOffset` fromIntegral sz
            Just succAddr

-- We explicitly match on all constructor patterns so that if/when new ones
-- are added this will break instead of having some default case that does
-- (potentially) the wrong thing on the new cases.
isUnconditionalJT :: Some (RI.JumpType arch) -> Bool
isUnconditionalJT (Some (RI.Return       cond    )) = isUnconditionalCond cond
isUnconditionalJT (Some (RI.IndirectJump cond    )) = isUnconditionalCond cond
isUnconditionalJT (Some (RI.AbsoluteJump cond _  )) = isUnconditionalCond cond
isUnconditionalJT (Some (RI.RelativeJump cond _ _)) = isUnconditionalCond cond
isUnconditionalJT (Some (RI.IndirectCall         )) = False
isUnconditionalJT (Some (RI.DirectCall {}        )) = False
isUnconditionalJT (Some (RI.NoJump               )) = False
-- This case is unfortunate, but no uninstrumentable blocks will ultimately need
-- the fallthrough information.
isUnconditionalJT (Some (RI.NotInstrumentable {} )) = False

isUnconditionalCond :: RI.JumpCondition -> Bool
isUnconditionalCond RI.Unconditional = True
isUnconditionalCond RI.Conditional   = False
