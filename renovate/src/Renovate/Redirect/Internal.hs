-- | Low-level code redirection helpers
--
-- This module is only exposed for testing purposes and is not an
-- external API.
module Renovate.Redirect.Internal ( redirectOriginalBlocks ) where

import           Data.Monoid
import qualified Data.Traversable as T

import           Prelude

import qualified Data.Macaw.Memory as MC

import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Redirect.Monad
import           Renovate.Redirect.LayoutBlocks.Types ( Status(..)
                                                      , ConcretePair
                                                      , LayoutPair(..) )

-- | Overwrite the entry points of each original block with a pointer
-- to the instrumented block, if possible.
--
-- It may not be possible if the original block is shorter than ~7
-- bytes.  We can improve this later, but for now, we will miss some
-- blocks.  We will generate a diagnostic for each one.
--
-- This is a low level helper mostly exposed for testing
redirectOriginalBlocks :: (MC.MemWidth w, Monad m, T.Traversable t, InstructionConstraints i a)
                       => t (ConcretePair i w)
                       -> RewriterT i a w m (t (ConcretePair i w))
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
redirectBlock :: (MC.MemWidth w, Monad m, InstructionConstraints i a)
              => ConcretePair i w
              -> RewriterT i a w m (ConcretePair i w)
redirectBlock input@(LayoutPair origBlock instrBlock Modified) = do
  isa <- askISA
  let origBlockSize = concreteBlockSize isa origBlock
      jmpInsns = isaMakeRelativeJumpTo isa (basicBlockAddress origBlock) (basicBlockAddress instrBlock)
      jmpSize = instructionStreamSize isa jmpInsns
  -- FIXME: Can we do this check at a higher level and earlier? It would
  -- probably fit very well in the top-level redirect loop
  case origBlockSize < jmpSize of
    True -> do
      logDiagnostic $ BlockTooSmallForRedirection origBlockSize jmpSize (basicBlockAddress origBlock)
                        (show origBlock ++ " |-> " ++ show instrBlock)
      return input
    False -> do
      let padding = isaMakePadding isa (origBlockSize - jmpSize)
          origBlock' = origBlock { basicBlockInstructions = jmpInsns <> padding }
      return (LayoutPair origBlock' instrBlock Modified)
redirectBlock unmodified@(LayoutPair _ _ Unmodified) = return unmodified
