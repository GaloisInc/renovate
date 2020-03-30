{-# LANGUAGE FlexibleContexts #-}
-- | Low-level code redirection helpers
--
-- This module is only exposed for testing purposes and is not an
-- external API.
module Renovate.Redirect.Internal ( redirectOriginalBlocks ) where

import qualified Data.Traversable as T

import           Prelude

import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Redirect.Monad
import           Renovate.Redirect.LayoutBlocks.Types

-- | Overwrite the entry points of each original block with a pointer
-- to the instrumented block, if possible.
--
-- It may not be possible if the original block is shorter than ~7
-- bytes.  We can improve this later, but for now, we will miss some
-- blocks.  We will generate a diagnostic for each one.
--
-- This is a low level helper mostly exposed for testing
redirectOriginalBlocks :: (Monad m, T.Traversable t, InstructionConstraints arch)
                       => t (WithProvenance ConcretizedBlock arch)
                       -> RewriterT arch m (t (WithProvenance ConcretizedBlock arch))
redirectOriginalBlocks = T.traverse redirectBlock

-- | Given an original 'ConcreteBlock' (in the 'WithProvenance' wrapper) and an
-- instrumented 'ConcretizedBlock', rewrite the original block to redirect to the
-- instrumented version, if possible.
--
-- This function will generate diagnostics for blocks that cannot be
-- redirected.
--
-- Note that the address of the jump instruction is the address of the
-- original block (since it will be the first instruction).
redirectBlock :: (Monad m, InstructionConstraints arch)
              => WithProvenance ConcretizedBlock arch
              -> RewriterT arch m (WithProvenance ConcretizedBlock arch)
redirectBlock input =
  case rewriteStatus input of
    Modified ->
      case originalBlock input of
        cb@ConcreteBlock { concreteBlockAddress = addr
                         , concreteBlockRepr = repr
                         , concreteDiscoveryBlock = pb
                         } -> do
          let instrBlock = withoutProvenance input

          isa <- askISA
          let origBlockSize = blockSize isa cb
              jmpInsns = isaMakeRelativeJumpTo isa addr (concretizedBlockAddress instrBlock) repr
              jmpSize = instructionStreamSize isa jmpInsns
          -- FIXME: Can we do this check at a higher level and earlier? It would
          -- probably fit very well in the top-level redirect loop
          case origBlockSize < jmpSize of
            True -> do
              recordUnrelocatableSize
              logDiagnostic $ BlockTooSmallForRedirection isa jmpSize cb instrBlock
              return input
            False -> do
              let origBlock' = ConcreteBlock { concreteBlockInstructions = jmpInsns
                                             , concreteBlockAddress = addr
                                             , concreteBlockRepr = repr
                                             , concreteDiscoveryBlock = pb
                                             }
              return $ WithProvenance origBlock' instrBlock Modified
    Unmodified -> return input
    Immutable -> return input
    Subsumed ->
      -- The consumer ignores the concrete block of a Subsumed block, so we
      -- don't need to figure out how to redirect it
      return input
