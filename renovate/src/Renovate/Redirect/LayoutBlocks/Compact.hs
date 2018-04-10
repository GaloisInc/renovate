{-# LANGUAGE FlexibleContexts #-}
module Renovate.Redirect.LayoutBlocks.Compact (
  compactLayout
  ) where

import qualified GHC.Err.Located as L

import           Data.Ord ( Down(..) )
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Control.Exception ( assert )
import           Control.Monad.ST
import qualified Data.Foldable as F
import qualified Data.Heap as H
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Traversable as T
import           Data.Word ( Word64 )
import           Text.Printf ( printf )

import qualified System.Random.MWC as MWC

import qualified Data.Macaw.CFG as MM

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Redirect.Monad

import           Renovate.Redirect.LayoutBlocks.Types

-- | The address heap associates chunks of memory to addresses.  The
-- ordering of the heap is based on the size of the chunk of memory at
-- each address. It's a priority heap with larger addresses having
-- higher priority, hence @Down Int@ to sort decreasing on @Int@ size.
type AddressHeap arch = H.Heap (H.Entry (Down Int) (ConcreteAddress arch))

-- | Compute a concrete address for each 'SymbolicBlock'.
--
-- Right now, we use an inefficient encoding of jumps.  We could do
-- better later on.
compactLayout :: (Monad m, T.Traversable t, InstructionConstraints arch)
              => ConcreteAddress arch
              -- ^ Address to begin block layout of instrumented blocks
              -> LayoutStrategy
              -> t (SymbolicPair arch)
              -> RewriterT arch m [AddressAssignedPair arch]
compactLayout startAddr strat blocks = do
  h0 <- if strat == Parallel -- the parallel strategy is now a special case of
                             -- compact. In particular, we avoid allocating
                             -- the heap and we avoid sorting the input
                             -- blocklist.
           then return mempty
           else buildAddressHeap startAddr blocks

  -- Augment all symbolic blocks such that fallthrough behavior is explicitly
  -- represented with symbolic unconditional jumps.
  --
  -- We need this so that we can re-arrange them and preserve the fallthrough
  -- behavior of blocks ending in conditional jumps (or non-jumps).
  -- traceM (show (PD.vcat (map PD.pretty (L.sortOn (basicBlockAddress . lpOrig) (F.toList blocks)))))
  mem     <- askMem
  let (modifiedBlocks, unmodifiedBlocks) = L.partition (\(SymbolicPair b) -> lpStatus b == Modified)
                                                       (F.toList blocks)
  blocks' <- reifyFallthroughSuccessors mem modifiedBlocks blocks

  -- Either, a) Sort all of the instrumented blocks by size
  --         b) Randomize the order of the blocks.
  --         c) Use the input order exactly
  -- The (a) will give a more optimal answer, but (b) will provide some
  -- synthetic diversity at the cost of optimality. (c) is for treating
  -- the parallel layout as a special case of compact.
  isa <- askISA
  let sortedBlocks =
        let newBlocks = F.toList ((lpNew . unSymbolicPair) <$> blocks') in
        case strat of
        Compact SortedOrder        -> L.sortOn    (bySize isa mem) newBlocks
        Compact (RandomOrder seed) -> randomOrder seed             newBlocks
        Parallel                   -> newBlocks

  -- Allocate an address for each block (falling back to startAddr if the heap
  -- can't provide a large enough space).
  symBlockAddrs <- allocateSymbolicBlockAddresses startAddr h0 sortedBlocks

  -- Traverse the original container and update it with the addresses allocated
  -- to each symbolic block.  This will have an irrefutable pattern match that
  -- is actually safe.
  --
  -- Note that we are assigning addresses to blocks', which has augmented the
  -- symbolic blocks with additional jumps to preserve fallthrough behavior.
  -- That is critical.
  T.traverse (assignConcreteAddress symBlockAddrs) (F.toList blocks' ++ unmodifiedBlocks)
  where
    bySize isa mem = Down . symbolicBlockSize isa mem startAddr

-- | Look up the concrete address assigned to each symbolic block and tag it
-- onto the tuple to create a suitable return value.
--
-- Every symbolic block is assumed to have been assigned an address at this
-- point.
assignConcreteAddress :: (Monad m, MM.MemWidth (MM.ArchAddrWidth arch))
                      => M.Map (SymbolicInfo arch) (ConcreteAddress arch)
                      -> SymbolicPair arch
                      -> RewriterT arch m (AddressAssignedPair arch)
assignConcreteAddress assignedAddrs (SymbolicPair (LayoutPair cb sb Modified)) = do
  case M.lookup (basicBlockAddress sb) assignedAddrs of
    Nothing -> L.error $ printf "Expected an assigned address for symbolic block %s (derived from concrete block %s)"
                                (show (basicBlockAddress sb))
                                (show (basicBlockAddress cb))
    Just addr -> return (AddressAssignedPair (LayoutPair cb (AddressAssignedBlock sb addr) Modified))
assignConcreteAddress _ (SymbolicPair (LayoutPair cb sb Unmodified)) =
  return (AddressAssignedPair (LayoutPair cb (AddressAssignedBlock sb (basicBlockAddress cb)) Unmodified))

allocateSymbolicBlockAddresses :: (Monad m, MM.MemWidth (MM.ArchAddrWidth arch))
                               => ConcreteAddress arch
                               -> AddressHeap arch
                               -> [SymbolicBlock arch]
                               -> RewriterT arch m (M.Map (SymbolicInfo arch) (ConcreteAddress arch))
allocateSymbolicBlockAddresses startAddr h0 blocksBySize = do
  isa <- askISA
  mem <- askMem
  (_, _, m) <- F.foldlM (allocateBlockAddress isa mem) (startAddr, h0, M.empty) blocksBySize
  return m

-- | Allocate an address for the given symbolic block.
--
-- If the block will fit into a space held in the address heap, allocate it to
-- that space (and return unused space to the heap).  Otherwise, place the
-- symbolic block into the new code section we are building up (based on the
-- @newTextAddr@).
--
-- Note that the 'SymbolicBlock' at this stage must have been augmented with its
-- final unconditional jump to preserve fallthrough control flow (we rely on the
-- size of the block to be correct).
allocateBlockAddress :: (MM.MemWidth (MM.ArchAddrWidth arch), Monad m)
                     => ISA arch
                     -> MM.Memory (MM.ArchAddrWidth arch)
                     -> (ConcreteAddress arch, AddressHeap arch, M.Map (SymbolicInfo arch) (ConcreteAddress arch))
                     -> SymbolicBlock arch
                     -> RewriterT arch m (ConcreteAddress arch, AddressHeap arch, M.Map (SymbolicInfo arch) (ConcreteAddress arch))
allocateBlockAddress isa mem (newTextAddr, h, m) sb =
  case H.viewMin h of
    Nothing -> return allocateNewTextAddr
    Just (H.Entry (Down size) addr, h')
      | size < fromIntegral sbSize -> return allocateNewTextAddr
      | otherwise -> do
          recordResuedBytes size
          return (allocateFromHeap size addr h')
  where
    addOff = addressAddOffset

    sbSize = symbolicBlockSize isa mem newTextAddr sb

    allocateNewTextAddr =
      let nextBlockStart = newTextAddr `addOff` fromIntegral sbSize
      in (nextBlockStart, h, M.insert (basicBlockAddress sb) newTextAddr m)

    allocateFromHeap allocSize addr h' =
      assert (allocSize >= fromIntegral sbSize) $ do
        let addr'      = addr `addOff` fromIntegral sbSize
            allocSize' = allocSize - fromIntegral sbSize
        case allocSize' of
          0 -> (newTextAddr, h', M.insert (basicBlockAddress sb) addr m)
          _ ->
            let h'' = H.insert (H.Entry (Down allocSize') addr') h'
            in (newTextAddr, h'', M.insert (basicBlockAddress sb) addr m)


-- | Make the fallthrough behavior of our symbolic blocks explicit.
--
-- During the layout process, we are going to re-arrange blocks so that old
-- fallthrough behavior no longer works.  We make fallthroughs explicit (with
-- unconditional jumps).
--
-- A block has fallthrough behavior if it does not end in an unconditional jump.
reifyFallthroughSuccessors :: (Traversable t, Monad m, MM.MemWidth (MM.ArchAddrWidth arch), Traversable t')
                           => MM.Memory (MM.ArchAddrWidth arch)
                           -> t (SymbolicPair arch)
                           -- ^ The modified blocks
                           -> t' (SymbolicPair arch)
                           -- ^ All blocks (which we need to compute the fallthrough address index)
                           -> RewriterT arch m (t (SymbolicPair arch))
reifyFallthroughSuccessors mem modifiedBlocks allBlocks =
  T.traverse (addExplicitFallthrough mem symSuccIdx) modifiedBlocks
  where
    blist0 = F.toList (fmap (lpNew . unSymbolicPair) allBlocks)
    symSuccs | length blist0 > 1 = zip blist0 (tail blist0)
             | otherwise = []
    -- An index mapping the symbolic address of a symbolic basic block to the
    -- symbolic address of its successor (in program order).
    symSuccIdx = F.foldl' indexSymbolicSuccessors M.empty symSuccs
    indexSymbolicSuccessors m (symBlock, symSucc) =
      M.insert (basicBlockAddress symBlock) (basicBlockAddress symSucc) m

type SuccessorMap arch = M.Map (SymbolicInfo arch) (SymbolicInfo arch)

addExplicitFallthrough :: (Monad m, MM.MemWidth (MM.ArchAddrWidth arch))
                       => MM.Memory (MM.ArchAddrWidth arch)
                       -> SuccessorMap arch
                       -> SymbolicPair arch
                       -> RewriterT arch m (SymbolicPair arch)
addExplicitFallthrough mem symSucIdx pair@(SymbolicPair (LayoutPair cb sb Modified)) = do
  isa <- askISA
  -- We pass in a fake relative address since we don't need the resolution of
  -- relative jumps.  We just need the type of jump.
  --
  -- If the block ends in an unconditional jump, just return it unmodified.
  -- Otherwise, append an absolute jump to the correct location.
  let newPair = SymbolicPair (LayoutPair cb (appendUnconditionalJump isa symSucIdx cb sb) Modified)
  case isaJumpType isa lastInsn mem fakeAddress of
    br | isUnconditional br -> return pair
       | otherwise          -> return newPair
  where
    -- We explicitly match on all constructor patterns so that if/when new ones
    -- are added this will break instead of having some default case that does
    -- (potentially) the wrong thing on the new cases.
    isUnconditional (Return                        ) = True
    isUnconditional (IndirectJump Unconditional    ) = True
    isUnconditional (AbsoluteJump Unconditional _  ) = True
    isUnconditional (RelativeJump Unconditional _ _) = True
    isUnconditional (IndirectCall                  ) = False
    isUnconditional (DirectCall {}                 ) = False
    isUnconditional (NoJump                        ) = False
    isUnconditional (IndirectJump Conditional      ) = False
    isUnconditional (AbsoluteJump Conditional _    ) = False
    isUnconditional (RelativeJump Conditional _ _  ) = False
    fakeAddress = concreteFromAbsolute 0
    lastInsn
      | null (basicBlockInstructions sb) = L.error (printf "Empty block for symbolic block %s (derived from block %s)"
                                                           (show (basicBlockAddress sb))
                                                           (show (basicBlockAddress cb)))
      | otherwise = projectInstruction $ last (basicBlockInstructions sb)
addExplicitFallthrough _ _ pair@(SymbolicPair (LayoutPair _ _ Unmodified)) = return pair

appendUnconditionalJump :: (MM.MemWidth (MM.ArchAddrWidth arch))
                        => ISA arch
                        -> SuccessorMap arch
                        -> ConcreteBlock arch
                        -> SymbolicBlock arch
                        -> SymbolicBlock arch
appendUnconditionalJump isa symSucIdx cb sb =
  case M.lookup (basicBlockAddress sb) symSucIdx of
    Nothing -> L.error (printf "Expected a successor block for symbolic block %s (derived from block %s)"
                               (show (basicBlockAddress sb))
                               (show (basicBlockAddress cb)))
    Just symSucc ->
      let insns = isaMakeSymbolicJump isa (symbolicAddress symSucc)
      in sb { basicBlockInstructions = basicBlockInstructions sb ++ insns }


buildAddressHeap :: (MM.MemWidth (MM.ArchAddrWidth arch), Foldable t, Monad m)
                 => ConcreteAddress arch
                 -> t (SymbolicPair arch)
                 -> RewriterT arch m (AddressHeap arch)
buildAddressHeap startAddr blocks = do
  isa <- askISA
  let dummyJump = isaMakeRelativeJumpTo  isa startAddr startAddr
      jumpSize = fromIntegral $ sum (map (isaInstructionSize isa) dummyJump)
  return $ F.foldl' (addOriginalBlock isa jumpSize) H.empty blocks

-- | Add the available space in a 'ConcreteBlock' to the heap
--
-- We subtract out the space required to redirect execution of the block to its
-- instrumented version.
--
-- NOTE: We only add blocks that have been *modified*.  If a block is
-- unmodified, overwriting it would throw away code, as we don't lay out
-- duplicates of unmodified blocks.
addOriginalBlock :: (MM.MemWidth (MM.ArchAddrWidth arch))
                 => ISA arch
                 -> Word64
                 -> AddressHeap arch
                 -> SymbolicPair arch
                 -> AddressHeap arch
addOriginalBlock isa jumpSize h (SymbolicPair (LayoutPair cb _ status))
  | bsize > jumpSize && status == Modified =
    H.insert (H.Entry (Down spaceSize) addr) h
  | otherwise = h
  where
    bsize     = concreteBlockSize isa cb
    spaceSize :: Int
    spaceSize = fromIntegral (bsize - jumpSize)
    addr      = basicBlockAddress cb `addressAddOffset` fromIntegral jumpSize

randomOrder :: RandomSeed -> [a] -> [a]
randomOrder seed initial = runST $ do
  gen      <- MWC.initialize seed
  vec      <- V.thaw (V.fromList initial)
  finalVec <- go gen 0 vec >>= V.freeze
  return (V.toList finalVec)
  where
  -- This looks like a bit of a mess, but it's actually just the fisher-yates
  -- inplace shuffle.
  go :: MWC.GenST s -> Int -> MV.STVector s a -> ST s (MV.STVector s a)
  go g i vec
    | i >= MV.length vec - 1 = return vec
    | otherwise = do
      j <- MWC.uniformR (i,MV.length vec-1) g
      MV.swap vec i j
      go g (i+1) vec

{- Note [Design]

The idea is that we want to re-use space in the original basic blocks to hold
new (instrumented) basic blocks.  We'll create a heap of all of the space
available in the original blocks and allocate that space to instrumented blocks.
Any instrumented blocks that cannot fit will be added to a new section.

Before we start, we need to augment each basic block that has a fallthrough case
with an unconditional jump to the correct block.  This means adding an
instruction to the end with a symbolic target.  We need to pre-process all
blocks to find fallthrough cases.

Steps:

1) Add all of the original basic blocks to the heap, subtracting the space
   required to add the redirection jump at the beginning.  If an original block
   is smaller than the size of a redirection jump, don't bother to add it.

2) Order the instrumented blocks by their size in bytes (with the largest blocks
   first).

3) Pull out the largest space available in the heap.  If it can fit the current
   instrumented block (plus space for an unconditional jump), allocate that
   address and return unused storage to the heap.



-}
