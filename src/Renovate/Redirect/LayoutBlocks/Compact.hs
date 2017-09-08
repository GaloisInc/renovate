module Renovate.Redirect.LayoutBlocks.Compact (
  compactLayout
  ) where

import qualified GHC.Err.Located as L

import           Control.Exception ( assert )
import qualified Data.Foldable as F
import qualified Data.Heap as H
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Traversable as T
import           Data.Word ( Word64 )
import           Text.Printf ( printf )

import qualified Data.Macaw.Memory as MM

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Redirect.Monad

-- | The address heap associates chunks of memory to addresses.  The ordering of
-- the heap is based on the size of the chunk of memory at each address.  The
-- sizes are stored as negative values so that taking the minimum element of the
-- heap yields the region with the largest amount of space left.
type AddressHeap w = H.Heap (H.Entry Int (RelAddress w))

-- | Compute a concrete address for each 'SymbolicBlock'.
--
-- Right now, we use an inefficient encoding of jumps.  We could do
-- better later on.
compactLayout :: (Monad m, T.Traversable t, Show (i a), MM.MemWidth w)
              => MM.Memory w
              -> RelAddress w
              -- ^ Address to begin block layout of instrumented blocks
              -> t (ConcreteBlock i w, SymbolicBlock i a w)
              -> RewriterT i a w m (t (ConcreteBlock i w, SymbolicBlock i a w, RelAddress w))
compactLayout mem startAddr blocks = do
  h0 <- buildAddressHeap startAddr (fmap fst blocks)

  -- Augment all symbolic blocks such that fallthrough behavior is explicitly
  -- represented with symbolic unconditional jumps.
  --
  -- We need this so that we can re-arrange them and preserve the fallthrough
  -- behavior of blocks ending in conditional jumps (or non-jumps).
  blocks' <- reifyFallthroughSuccessors mem blocks

  -- Sort all of the instrumented blocks by size
  isa <- askISA
  let blocksBySize = L.sortOn (bySize isa) (F.toList (fmap snd blocks'))

  -- Allocate an address for each block (falling back to startAddr if the heap
  -- can't provide a large enough space).
  symBlockAddrs <- allocateSymbolicBlockAddresses startAddr h0 blocksBySize

  -- Traverse the original container and update it with the addresses allocated
  -- to each symbolic block.  This will have an irrefutable pattern match that
  -- is actually safe.
  --
  -- Note that we are assigning addresses to blocks', which has augmented the
  -- symbolic blocks with additional jumps to preserve fallthrough behavior.
  -- That is critical.
  T.traverse (assignConcreteAddress symBlockAddrs) blocks'
  where
    -- We negate the size so that the largest (in magnitude) values come first
    bySize isa = negate . symbolicBlockSize isa startAddr

-- | Look up the concrete address assigned to each symbolic block and tag it
-- onto the tuple to create a suitable return value.
--
-- Every symbolic block is assumed to have been assigned an address at this
-- point.
assignConcreteAddress :: (Monad m)
                      => M.Map (SymbolicInfo w) (RelAddress w)
                      -> (ConcreteBlock i w, SymbolicBlock i a w)
                      -> RewriterT i a w m (ConcreteBlock i w, SymbolicBlock i a w, RelAddress w)
assignConcreteAddress assignedAddrs (cb, sb) = do
  case M.lookup (basicBlockAddress sb) assignedAddrs of
    Nothing -> L.error $ printf "Expected an assigned address for symbolic block %d (derived from concrete block %d)" (show (basicBlockAddress sb)) (show (basicBlockAddress cb))
    Just addr -> return (cb, sb, addr)

allocateSymbolicBlockAddresses :: (Monad m, MM.MemWidth w)
                               => RelAddress w
                               -> AddressHeap w
                               -> [SymbolicBlock i a w]
                               -> RewriterT i a w m (M.Map (SymbolicInfo w) (RelAddress w))
allocateSymbolicBlockAddresses startAddr h0 blocksBySize = do
  isa <- askISA
  let (_, _, m) = F.foldl' (allocateBlockAddress isa) (startAddr, h0, M.empty) blocksBySize
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
allocateBlockAddress :: (MM.MemWidth w)
                     => ISA i a w
                     -> (RelAddress w, AddressHeap w, M.Map (SymbolicInfo w) (RelAddress w))
                     -> SymbolicBlock i a w
                     -> (RelAddress w, AddressHeap w, M.Map (SymbolicInfo w) (RelAddress w))
allocateBlockAddress isa (newTextAddr, h, m) sb =
  case H.viewMin h of
    Nothing -> allocateNewTextAddr
    Just (H.Entry negSize addr, h')
      | negate negSize < fromIntegral sbSize -> allocateNewTextAddr
      | otherwise -> allocateFromHeap (negate negSize) addr h'
  where
    sbSize = symbolicBlockSize isa newTextAddr sb

    allocateNewTextAddr =
      let nextBlockStart = newTextAddr `addressAddOffset` fromIntegral sbSize
      in (nextBlockStart, h, M.insert (basicBlockAddress sb) newTextAddr m)

    allocateFromHeap allocSize addr h' =
      assert (allocSize >= fromIntegral sbSize) $ do
        let addr' = addr `addressAddOffset` fromIntegral sbSize
            allocSize' = allocSize - fromIntegral sbSize
        case allocSize' of
          0 -> (newTextAddr, h', M.insert (basicBlockAddress sb) addr m)
          _ ->
            let h'' = H.insert (H.Entry (negate allocSize') addr') h'
            in (newTextAddr, h'', M.insert (basicBlockAddress sb) addr m)


-- | Make the fallthrough behavior of our symbolic blocks explicit.
--
-- During the layout process, we are going to re-arrange blocks so that old
-- fallthrough behavior no longer works.  We make fallthroughs explicit (with
-- unconditional jumps).
--
-- A block has fallthrough behavior if it does not end in an unconditional jump.
reifyFallthroughSuccessors :: (Traversable t, Monad m, MM.MemWidth w)
                           => MM.Memory w
                           -> t (ConcreteBlock i w, SymbolicBlock i a w)
                           -> RewriterT i a w m (t (ConcreteBlock i w, SymbolicBlock i a w))
reifyFallthroughSuccessors mem blocks = T.traverse (addExplicitFallthrough mem symSuccIdx) blocks
  where
    blist0 = F.toList (fmap snd blocks)
    symSuccs | length blist0 > 1 = zip blist0 (tail blist0)
             | otherwise = []
    -- An index mapping the symbolic address of a symbolic basic block to the
    -- symbolic address of its successor (in program order).
    symSuccIdx = F.foldl' indexSymbolicSuccessors M.empty symSuccs
    indexSymbolicSuccessors m (symBlock, symSucc) =
      M.insert (basicBlockAddress symBlock) (basicBlockAddress symSucc) m

addExplicitFallthrough :: (Monad m, MM.MemWidth w)
                       => MM.Memory w
                       -> M.Map (SymbolicInfo w) (SymbolicInfo w)
                       -> (ConcreteBlock i w, SymbolicBlock i a w)
                       -> RewriterT i a w m (ConcreteBlock i w, SymbolicBlock i a w)
addExplicitFallthrough mem symSucIdx pair@(cb, sb) = do
  isa <- askISA
  -- We pass in a fake relative address since we don't need the resolution of
  -- relative jumps.  We just need the type of jump.
  --
  -- If the block ends in an unconditional jump, just return it unmodified.
  -- Otherwise, append an absolute jump to the correct location.
  case isaJumpType isa lastInsn mem fakeAddress of
    Return                         -> return pair
    IndirectJump Unconditional     -> return pair
    AbsoluteJump Unconditional _   -> return pair
    RelativeJump Unconditional _ _ -> return pair
    IndirectCall                   -> return (cb, appendUnconditionalJump isa symSucIdx cb sb)
    DirectCall {}                  -> return (cb, appendUnconditionalJump isa symSucIdx cb sb)
    NoJump                         -> return (cb, appendUnconditionalJump isa symSucIdx cb sb)
    IndirectJump Conditional       -> return (cb, appendUnconditionalJump isa symSucIdx cb sb)
    AbsoluteJump Conditional _     -> return (cb, appendUnconditionalJump isa symSucIdx cb sb)
    RelativeJump Conditional _ _   -> return (cb, appendUnconditionalJump isa symSucIdx cb sb)
  where
    fakeAddress = firstRelAddress 0 0
    lastInsn
      | null (basicBlockInstructions sb) = L.error (printf "Empty block for symbolic block %s (derived from block %s)" (show (basicBlockAddress sb)) (show (basicBlockAddress cb)))
      | otherwise = projectInstruction $ last (basicBlockInstructions sb)

appendUnconditionalJump :: (MM.MemWidth w)
                        => ISA i a w
                        -> M.Map (SymbolicInfo w) (SymbolicInfo w)
                        -> ConcreteBlock i w
                        -> SymbolicBlock i a w
                        -> SymbolicBlock i a w
appendUnconditionalJump isa symSucIdx cb sb =
  case M.lookup (basicBlockAddress sb) symSucIdx of
    Nothing -> L.error (printf "Expected a successor block for symbolic block %s (derived from block %s)" (show (basicBlockAddress sb)) (show (basicBlockAddress cb)))
    Just symSucc ->
      let insns = isaMakeSymbolicJump isa (symbolicAddress symSucc)
      in sb { basicBlockInstructions = basicBlockInstructions sb ++ insns }


buildAddressHeap :: (MM.MemWidth w, Foldable t, Monad m)
                 => RelAddress w
                 -> t (ConcreteBlock i w)
                 -> RewriterT i a w m (AddressHeap w)
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
-- We actually insert the negation of the available space into the heap so that
-- extracting the minimum value yields the largest block possible.
addOriginalBlock :: (MM.MemWidth w)
                 => ISA i a w
                 -> Word64
                 -> AddressHeap w
                 -> ConcreteBlock i w
                 -> AddressHeap w
addOriginalBlock isa jumpSize h cb
  | bsize > jumpSize =
    H.insert (H.Entry (negate spaceSize) addr) h
  | otherwise = h
  where
    bsize = concreteBlockSize isa cb
    spaceSize :: Int
    spaceSize = fromIntegral (bsize - jumpSize)
    addr = basicBlockAddress cb `addressAddOffset` fromIntegral jumpSize

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