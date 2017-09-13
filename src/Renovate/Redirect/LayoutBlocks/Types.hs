module Renovate.Redirect.LayoutBlocks.Types (
  LayoutStrategy(..),
  CompactOrdering(..),
  RandomSeed
  ) where

import qualified Data.Vector.Unboxed as V
import           Data.Word ( Word32 )

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

type RandomSeed = V.Vector Word32

data CompactOrdering
  = SortedOrder            -- ^ Sort by block size
  | RandomOrder RandomSeed -- ^ seed for the randomization
  deriving (Read, Show, Eq, Ord)
