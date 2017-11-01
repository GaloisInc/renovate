{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Renovate.Redirect.LayoutBlocks.Types (
  LayoutStrategy(..),
  CompactOrdering(..),
  LayoutPair(..),
  SymbolicPair,
  AddressAssignedPair,
  ConcretePair,
  Status(..),
  RandomSeed
  ) where

import qualified Data.Vector.Unboxed as V
import           Data.Word ( Word32 )
import           Renovate.BasicBlock
import qualified Data.Macaw.Memory as MM
import qualified Data.Text.Prettyprint.Doc as PD

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

instance ( MM.MemWidth w
         , PD.Pretty (i a)
         , PD.Pretty (i ())
         ) => PD.Pretty (SymbolicPair i a w) where
  pretty (LayoutPair o n _) = ppBlocks projectInstruction o n

instance ( MM.MemWidth w
         , PD.Pretty (i ())
         ) => PD.Pretty (ConcretePair i w) where
  pretty (LayoutPair o n _) = ppBlocks id o n

ppBlocks :: ( PD.Pretty (i1 a1)
            , PD.Pretty (i1 a2)
            , PD.Pretty addr1
            ) => (i2 b -> i1 a2) -> BasicBlock addr1 i1 a1 -> BasicBlock addr2 i2 b -> PD.Doc ann
ppBlocks f o n = PD.vcat $ [ PD.pretty (basicBlockAddress o) PD.<> PD.pretty ":" ] ++
                           ppInsnLists origInsns newInsns
  where
  origInsns = basicBlockInstructions o
  newInsns  = f <$> basicBlockInstructions n

-- | This lays out the instruction lists side by side with a divider (eg., |)
-- between the columns. The instruction sequences do not need to be the same
-- length.
ppInsnLists :: ( PD.Pretty (i a)
               , PD.Pretty (i b)
               ) => [i a] -> [i b] -> [PD.Doc ann]
ppInsnLists xs ys = go xs ys

  where
  divider           = PD.pretty "|"
  maxLen            = maxOrDefault 0 (map (length . show . PD.pretty) xs)
  spacing           = 0
  maxOrDefault d [] = d
  maxOrDefault _ zs = maximum zs
  go (o:os)    (n:ns)      =
       (PD.pretty o PD.<+> PD.indent (maxLen - curLen + spacing) (divider PD.<+> PD.pretty n)) : go os ns
       where
       curLen = length (show (PD.pretty o))
  go (os@(_:_)) []         = map PD.pretty os
  go []         (ns@(_:_)) = (PD.indent (maxLen + spacing + 1)) <$> map (\x -> divider PD.<+> PD.pretty x) ns
  go []         []         = [PD.emptyDoc]


data Status
  = Modified
  | Unmodified
  deriving (Eq, Ord, Read, Show)

type SymbolicPair         i a w = LayoutPair (SymbolicBlock        i a w) i w
type AddressAssignedPair  i a w = LayoutPair (AddressAssignedBlock i a w) i w
type ConcretePair         i   w = LayoutPair (ConcreteBlock        i   w) i w
