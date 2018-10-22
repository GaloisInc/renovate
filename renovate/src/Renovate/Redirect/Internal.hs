{-# LANGUAGE FlexibleContexts #-}
-- | Low-level code redirection helpers
--
-- This module is only exposed for testing purposes and is not an
-- external API.
module Renovate.Redirect.Internal ( redirectOriginalBlocks ) where

import           Data.Monoid
import qualified Data.Traversable as T

import           Prelude

import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Redirect.Monad
import           Renovate.Redirect.LayoutBlocks.Types ( Status(..)
                                                      , ConcretePair(..)
                                                      , LayoutPair(..) )

-- | Overwrite the entry points of each original block with a pointer
-- to the instrumented block, if possible.
--
-- It may not be possible if the original block is shorter than ~7
-- bytes.  We can improve this later, but for now, we will miss some
-- blocks.  We will generate a diagnostic for each one.
--
-- This is a low level helper mostly exposed for testing
redirectOriginalBlocks :: (Monad m, T.Traversable t, InstructionConstraints arch)
                       => t (ConcretePair arch)
                       -> RewriterT arch m (t (ConcretePair arch))
redirectOriginalBlocks = T.traverse redirectBlock

-- | Given an original 'ConcreteBlock' and an instrumented
-- 'ConcreteBlock', rewrite the original block to redirect to the
-- instrumented version, if possible.
--
-- This function will generate diagnostics for blocks that cannot be
-- redirected.
--
-- Note that the address of the jump instruction is the address of the
-- original block (since it will be the first instruction).
redirectBlock :: (Monad m, InstructionConstraints arch)
              => ConcretePair arch
              -> RewriterT arch m (ConcretePair arch)
redirectBlock input@(ConcretePair (LayoutPair origBlock instrBlock Modified)) = do
  isa <- askISA
  let origBlockSize = concreteBlockSize isa origBlock
      jmpInsns = isaMakeRelativeJumpTo isa (basicBlockAddress origBlock) (basicBlockAddress instrBlock)
      jmpSize = instructionStreamSize isa jmpInsns
  -- FIXME: Can we do this check at a higher level and earlier? It would
  -- probably fit very well in the top-level redirect loop
  case origBlockSize < jmpSize of
    True -> do
      recordUnrelocatableSize
      logDiagnostic $ BlockTooSmallForRedirection origBlockSize jmpSize (basicBlockAddress origBlock)
                        (show origBlock ++ " |-> " ++ show instrBlock)
      return input
    False -> do
      let origBlock' = origBlock { basicBlockInstructions = jmpInsns }
      return (ConcretePair (LayoutPair origBlock' instrBlock Modified))
redirectBlock unmodified@(ConcretePair (LayoutPair _ _ Unmodified)) = return unmodified
