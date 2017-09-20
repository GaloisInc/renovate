{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Renovate.Redirect.LayoutBlocks.Types (
  LayoutStrategy(..),
  CompactOrdering(..),
  LayoutPair(..),
  SymbolicPair,
  AddressAssignedPair,
  Status(..),
  RandomSeed
  ) where

import qualified Data.Vector.Unboxed as V
import           Data.Word ( Word32 )
import           Renovate.BasicBlock

-- | A type for selecting the strategy for laying out basic blocks in rewritten
-- binaries.
data LayoutStrategy = Parallel
                     -- ^ Lay instrumented blocks out in parallel with the
                     -- original basic blocks.  The excess space in the original
                     -- blocks will be filled with trap instructions.
                     -- Instrumented blocks will all be placed in a new text
                     -- section.
                     | Compact CompactOrdering
                     -- ^ Lay blocks out more compactly by re-using space in
                     -- original basic blocks to hold instrumented code.
                     -- Instrumented blocks that cannot fit in existing slack
                     -- space will be placed in a new text section.
                     --
                     -- Also takes an ordering, sorted or randomized.
                    deriving (Eq, Ord, Read, Show)

-- | Directly use the same seed type as the mwc-random package.
type RandomSeed = V.Vector Word32

data CompactOrdering
  = SortedOrder            -- ^ Sort by block size
  | RandomOrder RandomSeed -- ^ seed for the randomization
  deriving (Read, Show, Eq, Ord)

-- | A layout pair allows us to track original blocks and their (maybe)
-- rewritten version. The status field tells us if the block was actually
-- modified and the type of the rewritten block is a type parameter because at
-- some points in the algorithm it is symbolic and at other points it will be
-- concrete.
data LayoutPair block i w = LayoutPair
  { lpOrig   :: ConcreteBlock i w -- ^ the original block
  , lpNew    :: block             -- ^ the instrumented block
  , lpStatus :: Status            -- ^ allows us to track if the instrumentor changed the block.
  }

data Status
  = Modified
  | Unmodified

type SymbolicPair         i a w = LayoutPair (SymbolicBlock        i a w) i w
type AddressAssignedPair  i a w = LayoutPair (AddressAssignedBlock i a w) i w
