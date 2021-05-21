{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
-- | This module is the entry point for binary code redirection
module Renovate.Redirect (
  redirect,
  BlockMapping(..),
  RedirectionResult(..),
  -- * Rewriter Monad
  RM.runRedirectT,
  RM.RewriterState(..),
  RM.SectionInfo(..),
  RM.SymbolMap,
  RM.NewSymbolsMap
  ) where

import qualified Control.Monad.Catch as X
import           Control.Monad.Trans ( MonadIO )
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Ord ( comparing )

import           Prelude

import qualified Data.Macaw.CFG as MM

import           Renovate.Core.Address as RCA
import           Renovate.Core.BasicBlock as RCB
import qualified Renovate.Core.Layout as RCL
import           Renovate.Recovery ( BlockInfo )
import           Renovate.Redirect.Concretize
import           Renovate.Redirect.Internal
import qualified Renovate.Redirect.Monad as RM
import           Renovate.Rewrite ( InjectSymbolicInstructions )
import           Renovate.Recovery.SymbolMap ( NewSymbolsMap )

data BlockMapping arch =
  BlockMapping { forwardBlockMapping :: M.Map (ConcreteAddress arch) (ConcreteAddress arch)
               , backwardBlockMapping :: M.Map (ConcreteAddress arch) (ConcreteAddress arch)
               }

data RedirectionResult arch =
  RedirectionResult { rrRedirectedBlocks :: [ConcretizedBlock arch]
                    , rrInjectedBlocks :: [(SymbolicAddress arch, ConcreteAddress arch, BS.ByteString)]
                    , rrInjectedInstructions :: [(SymbolicAddress arch, ConcreteAddress arch, InjectConcreteInstructions arch)]
                    , rrConcretizedBlocks :: [RCL.WithProvenance ConcretizedBlock arch]
                    , rrSymbolicToConcreteMap ::M.Map (SymbolicAddress arch) (ConcreteAddress arch)
                    , rrBlockMapping :: BlockMapping arch
                    , rrNewSymbolsMap :: NewSymbolsMap arch
                    }

-- | Given a list of basic blocks with instructions of type @i@ with
-- annotation @a@ (which is fixed by the 'ISA' choice), rewrite the
-- blocks to redirect execution to alternate blocks that have been
-- instrumented with the provided @instrumentor@.  The instrumentor is
-- applied to each redirected basic block.  The new blocks are laid
-- out starting at the new start address, which will typically be in a
-- different section.
--
-- The output blocks are returned in order by address.
--
-- The function runs in an arbitrary 'Monad' to allow instrumentors to
-- carry around their own state.
--
redirect
  :: ( MM.MemWidth (MM.ArchAddrWidth arch)
     , MonadIO m
     , X.MonadThrow m
     )
  => BlockInfo arch
  -- ^ Information about all recovered blocks
  -> RCL.LayoutStrategy
  -> ConcreteAddress arch
  -- ^ The start address for the copied blocks
  -> [RCL.WithProvenance SymbolicBlock arch]
  -- ^ Basic blocks transformed by the client-provided instrumentor
  -> [(RCA.SymbolicAddress arch, BS.ByteString)]
  -- ^ Code injected at symbolic addresses as raw bytes
  -> [(RCA.SymbolicAddress arch, InjectSymbolicInstructions arch)]
  -- ^ Code injected at symbolic addresses as high-level instructions to be assembled
  -> RM.RedirectT lm arch m (RedirectionResult arch)
redirect blockInfo strat layoutAddr transformedBlocks injectedCode injectedInstructions = do
  (layout, symToConcAddrs) <- concretize strat layoutAddr transformedBlocks injectedCode injectedInstructions blockInfo
  let concretizedBlocks = RCL.programBlockLayout layout
  let paddingBlocks = RCL.layoutPaddingBlocks layout
  let injectedBlocks = RCL.injectedBlockLayout layout
  let injectedInsns = RCL.injectedInstructionLayout layout
  redirectedBlocks <- redirectOriginalBlocks concretizedBlocks

  let forwardMap = M.fromList (toBlockMapping concretizedBlocks)
  let reverseMap = toBackwardBlockMapping redirectedBlocks

  let sortedBlocks = L.sortBy (comparing concretizedBlockAddress) (fmap concretizePadding paddingBlocks ++ concatMap unPair (F.toList redirectedBlocks))
  let blockMapping = BlockMapping { forwardBlockMapping = forwardMap
                                  , backwardBlockMapping = reverseMap
                                  }

  nsm <- RM.getNewSymbolsMap
  return RedirectionResult { rrRedirectedBlocks = sortedBlocks
                           , rrInjectedBlocks = injectedBlocks
                           , rrInjectedInstructions = injectedInsns
                           , rrConcretizedBlocks = concretizedBlocks
                           , rrSymbolicToConcreteMap = symToConcAddrs
                           , rrBlockMapping = blockMapping
                           , rrNewSymbolsMap = nsm
                           }
  where
    concretizePadding :: PaddingBlock arch -> ConcretizedBlock arch
    concretizePadding pb =
      withPaddingInstructions pb $ \repr insns ->
        concretizedBlock (paddingBlockAddress pb) insns repr
    -- Convert a 'ConcreteBlock' into a 'ConcretizedBlock' by dropping its macaw
    -- block
    toConcretized :: ConcreteBlock arch -> ConcretizedBlock arch
    toConcretized cb =
      withConcreteInstructions cb $ \repr insns ->
        concretizedBlock (concreteBlockAddress cb) insns repr
    unPair wp =
      case RCL.rewriteStatus wp of
        RCL.Modified    -> [toConcretized (RCL.originalBlock wp), RCL.withoutProvenance wp]
        RCL.Unmodified  -> [toConcretized (RCL.originalBlock wp)]
        RCL.Immutable   -> [toConcretized (RCL.originalBlock wp)]
        RCL.Subsumed    -> [                                  RCL.withoutProvenance wp]


toBlockMapping :: [RCL.WithProvenance ConcretizedBlock arch] -> [(ConcreteAddress arch, ConcreteAddress arch)]
toBlockMapping wps =
  [ (concreteBlockAddress origBlock, concretizedBlockAddress concBlock)
  | wp <- wps
  , let origBlock = RCL.originalBlock wp
  , let concBlock = RCL.withoutProvenance wp
  ]

toBackwardBlockMapping :: [RCL.WithProvenance ConcretizedBlock arch]
                       -> M.Map (ConcreteAddress arch) (ConcreteAddress arch)
toBackwardBlockMapping ps = M.fromList
  [ (new, old)
  | RCL.WithProvenance cb sb status <- ps
  , let caddr = concreteBlockAddress cb
        saddr = concretizedBlockAddress sb
  , (new, old) <- [(caddr, caddr) | status /= RCL.Subsumed]
               ++ [(saddr, caddr) | RCL.changed status]
  ]

{- Note [Redirection]

(As of 2018-03-27 conathan believes this Note is out of date. For
example, (2) talks about ensuring that "fallthroughs in conditional
jumps continue to work", but Renovate.Redirect.LayoutBlocks.Compact
adds epilogues with absolute jumps to eliminate implicit fallthrough.)

The redirection is complex, but can be essentially broken down into
the following steps:

1) Create a symbolic index of all of the jumps in all of the basic
blocks.

2) Make a copy of each basic block; the copies will be placed in the
same order as the originals, but far away in the address space.  This
is necessary to let fallthroughs in conditional jumps continue to
work.  Note that we do not know the addresses of the new blocks at
this stage, since they might shift around as we apply the
instrumentation transformer.

3) Apply the instrumentor function to each copied basic block to
produce the fragile version.  After all of the blocks are transformed,
we can then lay them out and concretize their addresses.

4) Stitch control flow back together.  This involves fixing up all of
the relative symbolic jumps in the copied blocks to have their
concrete addresses (post instrumentation).

5) Rewrite the entry point of each of the original blocks that can
hold an absolute jump to the copied version.

6) Rewrite all of the absolute jumps in the program to point from old
blocks to new blocks.

Special handling for:

* Jump tables

Representation notes:

* We only need the addresses of basic blocks, as we can jump to those.
The addresses of individual instructions are not very important.  We
don't track them for individual instructions to avoid the headache of
ensuring their consistency with the basic block addresses.  We do need
the address of an individual instruction when resolving relative
jumps, though...

How can we express "this instruction really points to symbolic address
X"?  The problem is that we probably don't want to actually rewrite
instructions up front (in the instrumented blocks).  Instead, we
probably want to maintain an index on the side that will tell us how
to tweak the instructions later on.

Note that we aren't going to be aggressively rewriting jumps in the
uninstrumented code at all.

-}

{- Note [PIC Jump Tables]

We do not allow users to rewrite blocks that end in an indirect jump (*not* an
indirect call).  This restriction is currently in place to preserve correctness.
In position-independent executables, these jumps are relative to the instruction
pointer.  If we rewrite the block, the address of the jump will change, but we
will not have updated the offset, which would break the binary.

The easiest fix is to prohibit these blocks from being rewritten at all, which
will preserve the IP.

In the future, we will attempt to rewrite patterns of indirect jump that we can
recognize, which will involve updating jump tables and verifying that we have
not broken anything else.

-}
