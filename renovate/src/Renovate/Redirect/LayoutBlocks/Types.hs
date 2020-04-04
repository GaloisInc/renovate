{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Renovate.Redirect.LayoutBlocks.Types (
  LayoutStrategy(..),
  Allocator(..),
  CompactOrdering(..),
  Grouping(..),
  TrampolineStrategy(..),
  LayoutPair(..),
  ConcretePair(..),
  RewritePair(..),
  toRewritePair,
  Status(..),
  changed,
  changeable,
  Layout(..),
  WithProvenance(..),
  RandomSeed
  ) where

import           Control.Monad ( guard )
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as V
import           Data.Word ( Word32 )

import qualified Data.Macaw.CFG as MC
import           Renovate.Address ( ConcreteAddress, SymbolicAddress )
import           Renovate.BasicBlock

-- | A type for selecting the which addresses are available for laying out
-- basic blocks in rewritten binaries.
data Allocator       = Parallel
                     -- ^ Lay instrumented blocks out in parallel with the
                     -- original basic blocks.  The excess space in the original
                     -- blocks will be filled with trap instructions.
                     -- Instrumented blocks will all be placed in a new text
                     -- section.
                     | Randomized RandomSeed
                     -- ^ Lay instrumented blocks randomly in a new text section.
                     -- The excess space in the original blocks will be filled with
                     -- trap instructions.
                     | Compact CompactOrdering
                     -- ^ Lay blocks out more compactly by re-using space in
                     -- original basic blocks to hold instrumented code.
                     -- Instrumented blocks that cannot fit in existing slack
                     -- space will be placed in a new text section.
                     --
                     -- Also takes an ordering, sorted or randomized.
                    deriving (Eq, Ord, Read, Show)

-- | Specifies the layout grouping for instrumented blocks.
data Grouping =
  BlockGrouping
    -- ^ place instructions that are part of the same block adjacent
    -- to each other, but do not attempt to order or group the blocks
    -- themselves in any fashion.
  | LoopGrouping
    -- ^ place blocks that are part of the same loop adjacent to each
    -- other
  | FunctionGrouping
    -- ^ place blocks that are part of the same function adjacent to
    -- each other.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Directly use the same seed type as the mwc-random package.
type RandomSeed = V.Vector Word32

data CompactOrdering
  = SortedOrder            -- ^ Sort by block size
  | RandomOrder RandomSeed -- ^ seed for the randomization
  deriving (Read, Show, Eq, Ord)

-- | When we lay out a block at a new location, when should we install a
-- trampoline (that is, a jump to the new location or else a copy of the old
-- block) at the old location?
data TrampolineStrategy
  = AlwaysTrampoline
  -- ^ Always.
  | WholeFunctionTrampoline
  -- ^ Only for function entry points, or for blocks that participate in
  -- functions we couldn't completely move for one reason or another.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A type for selecting the strategy for laying out basic blocks in rewritten
-- binaries.
data LayoutStrategy = LayoutStrategy
  { allocator :: Allocator
  , grouping :: Grouping
  , trampolines :: TrampolineStrategy
  } deriving (Eq, Ord, Read, Show)

data Layout b arch =
  Layout { programBlockLayout :: [WithProvenance b arch]
         , layoutPaddingBlocks :: [PaddingBlock arch]
         , injectedBlockLayout :: [(SymbolicAddress arch, ConcreteAddress arch, BS.ByteString)]
         }

-- | A wrapper around a block type @b@ that tracks the provenance of the block
-- (i.e., the concrete block it came from and rewriter status info)
data WithProvenance b arch =
  WithProvenance { originalBlock :: ConcreteBlock arch
                 -- ^ The block from which the base block (i.e.,
                 -- 'withoutProvenance') was derived
                 , withoutProvenance :: b arch
                 -- ^ The payload for which we are tracking provenance; the
                 -- accessor discards the provenance information
                 , rewriteStatus :: Status
                 -- ^ A marker indicating whether or not the instrumentor
                 -- changed the block
                 }

data Status
  = Modified
  | Unmodified
  -- ^ This code hasn't been modified yet, but is eligible to be in the future
  -- if it turns out to be useful.
  | Immutable
  -- ^ This code shouldn't be modified by anyone, e.g. because it has control
  -- flow constructs we don't fully understand or isn't in the text section or
  -- similar.
  | Subsumed
  -- ^ This code has been modified, but we don't need to leave a redirection in
  -- its old location, because we believe we have already found and fixed up
  -- all jumps to this block.
  deriving (Eq, Ord, Read, Show)

-- | Has the code in the given block been changed in any way?
changed :: Status -> Bool
changed Modified = True
changed Unmodified = False
changed Immutable = False
changed Subsumed = True

-- | Is the code in the given block eligible to be changed further?
changeable :: Status -> Bool
changeable Modified = True
changeable Unmodified = True
changeable Immutable = False
changeable Subsumed = True

newtype ConcretePair arch = ConcretePair { unConcretePair :: LayoutPair (ConcreteBlock arch) arch }

-- RewritePair is basically a ConcretePair, but in a format that's more
-- friendly for sharing with the outside world. It also serves as a gatekeeper:
-- we export RewritePair from the package, but not ConcretePair, and since an
-- explicit conversion is required between them, this forces us to be conscious
-- about sending internal data to the outside world.

-- | A block, together with what it's been rewritten to. If 'rpNew' is
-- 'Nothing', this means that we kept the original code in the original
-- position.
data RewritePair arch = RewritePair
  { rpOrig :: ConcreteBlock arch
  , rpNew :: Maybe (ConcreteBlock arch)
  }

deriving instance Eq (Instruction arch ()) => Eq (RewritePair arch)
deriving instance (Show (Instruction arch ()), MC.MemWidth (MC.ArchAddrWidth arch)) => Show (RewritePair arch)

toRewritePair :: ConcretePair arch -> RewritePair arch
toRewritePair (ConcretePair (LayoutPair orig new status))
  = RewritePair orig (guard (changed status) *> Just new)
