{-# LANGUAGE FlexibleContexts #-}
module Renovate.Redirect.ReifyFallthrough (
  reifyFallthrough
  ) where

import qualified Data.List.NonEmpty as DLN
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.Memory as MM
import           Data.Parameterized.Some ( Some(..) )

import qualified Renovate.Address as RA
import qualified Renovate.BasicBlock as RB
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
                 -> RB.ExplicitFallthroughBlock arch
reifyFallthrough isa mem cb
  | isUnconditionalJT (RI.isaJumpType isa lastInsn mem lastInsnAddr) =
      RB.explicitFallthroughBlock (RB.concreteBlockAddress cb)
                                  (RB.concreteBlockInstructions cb)
                                  Nothing
  | otherwise =
      case RB.concreteDiscoveryBlock cb of
        Some pb -> do
          let sz = MD.blockSize pb
              succAddr = RB.concreteBlockAddress cb `RA.addressAddOffset` fromIntegral sz
            in RB.explicitFallthroughBlock (RB.concreteBlockAddress cb)
                                           (RB.concreteBlockInstructions cb)
                                           (Just succAddr)
  where
    (lastInsn, lastInsnAddr) = DLN.last (RB.instructionAddresses isa cb)

-- We explicitly match on all constructor patterns so that if/when new ones
-- are added this will break instead of having some default case that does
-- (potentially) the wrong thing on the new cases.
isUnconditionalJT :: RI.JumpType arch -> Bool
isUnconditionalJT (RI.Return       cond    ) = isUnconditionalCond cond
isUnconditionalJT (RI.IndirectJump cond    ) = isUnconditionalCond cond
isUnconditionalJT (RI.AbsoluteJump cond _  ) = isUnconditionalCond cond
isUnconditionalJT (RI.RelativeJump cond _ _) = isUnconditionalCond cond
isUnconditionalJT (RI.IndirectCall         ) = False
isUnconditionalJT (RI.DirectCall {}        ) = False
isUnconditionalJT (RI.NoJump               ) = False

isUnconditionalCond :: RI.JumpCondition -> Bool
isUnconditionalCond RI.Unconditional = True
isUnconditionalCond RI.Conditional   = False
