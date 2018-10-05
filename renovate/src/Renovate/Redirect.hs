{-# LANGUAGE FlexibleContexts #-}
-- | This module is the entry point for binary code redirection
module Renovate.Redirect (
  redirect,
  LayoutStrategy(..),
  ConcreteBlock,
  SymbolicBlock,
  BasicBlock(..),
  ConcreteAddress,
  SymbolicAddress,
  TaggedInstruction,
  -- * Rewriter Monad
  RM.runRewriter,
  RM.runRewriterT,
  RM.resumeRewriterT,
  RM.Diagnostic(..),
  RM.RewriterResult(..),
  RM.RewriterState(..),
  RM.SymbolMap,
  RM.NewSymbolsMap
  ) where

import           Control.Arrow ( (***) )
import           Control.Monad ( when )
import           Control.Monad.Trans ( lift )
import           Data.Maybe ( catMaybes )
import qualified Data.Foldable as F
import qualified Data.List as L
import           Data.Ord ( comparing )
import qualified Data.Traversable as T

import           Prelude

import qualified Data.Macaw.CFG as MM

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Recovery ( BlockInfo, isIncompleteBlockAddress, biOverlap )
import           Renovate.Recovery.Overlap ( disjoint )
import           Renovate.Redirect.Concretize
import           Renovate.Redirect.LayoutBlocks.Types ( LayoutStrategy(..)
                                                      , Status(..)
                                                      , ConcretePair(..)
                                                      , SymbolicPair(..)
                                                      , LayoutPair(..) )
import           Renovate.Redirect.Internal
import qualified Renovate.Redirect.Monad as RM

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
redirect :: (Monad m, InstructionConstraints arch)
         => ISA arch
         -- ^ Information about the ISA in use
         -> BlockInfo arch
         -- ^ Information about all recovered blocks
         -> ConcreteAddress arch
         -- ^ start of text section
         -> ConcreteAddress arch
         -- ^ end of text section
         -> (SymbolicBlock arch -> m (Maybe [TaggedInstruction arch (InstructionAnnotation arch)]))
         -- ^ Instrumentor
         -> MM.Memory (MM.ArchAddrWidth arch)
         -- ^ The memory space
         -> LayoutStrategy
         -> ConcreteAddress arch
         -- ^ The start address for the copied blocks
         -> [(ConcreteBlock arch, SymbolicBlock arch)]
         -- ^ Symbolized basic blocks
         -> RM.RewriterT arch m ([ConcreteBlock arch], [ConcreteBlock arch])
redirect isa blockInfo textStart textEnd instrumentor mem strat layoutAddr baseSymBlocks = do
  -- traceM (show (PD.vcat (map PD.pretty (L.sortOn (basicBlockAddress . fst) (F.toList baseSymBlocks)))))
  transformedBlocks <- T.forM baseSymBlocks $ \(cb, sb) -> do
    -- We only want to instrument blocks that:
    --
    -- 1. Live in the .text
    -- 2. Do not rely on their location (e.g. via PIC jumps)
    -- 3. Do not reside in incomplete functions (where unknown control flow might break our assumptions)
    -- 4. Do not overlap other blocks (which are hard for us to rewrite)
    --
    -- Also, see Note [PIC Jump Tables]
    case and [ textStart <= basicBlockAddress cb
             , basicBlockAddress cb < textEnd
             , isRelocatableTerminatorType (terminatorType isa mem cb)
             , not (isIncompleteBlockAddress blockInfo (basicBlockAddress cb))
             , disjoint isa (biOverlap blockInfo) cb
             ] of
     True ->  do
       insns' <- lift $ instrumentor sb
       case insns' of
         Nothing      -> return (SymbolicPair (LayoutPair cb sb Unmodified))
         Just insns'' -> return (SymbolicPair (LayoutPair cb sb { basicBlockInstructions = insns'' } Modified))
     False -> do
       when (not (isRelocatableTerminatorType (terminatorType isa mem cb))) $ do
         RM.recordUnrelocatableTermBlock
       when (isIncompleteBlockAddress blockInfo (basicBlockAddress cb)) $ do
         RM.recordIncompleteBlock
       return (SymbolicPair (LayoutPair cb sb Unmodified))
  concretizedBlocks <- concretize strat layoutAddr transformedBlocks
  RM.recordBlockMap (toBlockMapping concretizedBlocks)
  redirectedBlocks <- redirectOriginalBlocks concretizedBlocks
  let sorter = L.sortBy (comparing basicBlockAddress)
  return $ (sorter *** sorter . catMaybes) (unzip (map toPair (F.toList redirectedBlocks)))
  where
    toPair (ConcretePair (LayoutPair cb sb Modified))   = (cb, Just sb)
    toPair (ConcretePair (LayoutPair cb _  Unmodified)) = (cb, Nothing)

toBlockMapping :: [ConcretePair arch] -> [(ConcreteAddress arch, ConcreteAddress arch)]
toBlockMapping ps =
  [ (basicBlockAddress (lpOrig lp), basicBlockAddress (lpNew lp))
  | ConcretePair lp <- ps
  ]

isRelocatableTerminatorType :: JumpType arch -> Bool
isRelocatableTerminatorType jt =
  case jt of
    IndirectJump {} -> False
    _ -> True

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
