{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Convert collections of basic blocks (specifically 'ConcreteBlock's) to
-- contiguous regions of bytes.
module Renovate.BasicBlock.Assemble
( assembleBlocks,
  BlockAssemblyException(..)
) where

import           Control.Applicative
import           Control.Exception ( assert )
import           Control.Monad ( unless )
import qualified Control.Monad.Catch as C
import qualified Control.Monad.State.Strict as St
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.List as L
import           Data.Monoid
import qualified Data.Traversable as T
import           Data.Typeable ( Typeable )

import           Prelude

import qualified Data.Macaw.Memory as MM

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA

import qualified Data.Text.Prettyprint.Doc as PD

data BlockAssemblyException where
  -- A discontiguous block was starting with the given concrete block
  DiscontiguousBlocks         :: forall i w
                               . (PD.Pretty (i ()), Show (i ()), Typeable (i ()), MM.MemWidth w)
                              => ConcreteBlock i w -> BlockAssemblyException

  UnexpectedMemoryContents    :: forall w
                               . (MM.MemWidth w)
                              => MM.MemSegmentOff w -> BlockAssemblyException
  AssemblyError               :: C.SomeException -> BlockAssemblyException

  BlockOverlappingRedirection :: forall i w
                               . (PD.Pretty (i ()), Show (i ()), Typeable (i ()), MM.MemWidth w)
                              => ConcreteBlock i w -> BlockAssemblyException

  OverlayBlockNotContained    :: forall i w
                               . (PD.Pretty (i ()), Show (i ()), Typeable (i ()), MM.MemWidth w)
                              => ConcreteBlock i w -> BlockAssemblyException

deriving instance Show BlockAssemblyException

instance PD.Pretty BlockAssemblyException where
  pretty (DiscontiguousBlocks cb) =
    PD.pretty "DiscontiguousBlocks:" PD.<+> PD.pretty cb
  pretty (UnexpectedMemoryContents seg) =
    PD.pretty "UnexpectedMemoryContents:" PD.<+> PD.pretty (show seg)
  pretty (AssemblyError e) = PD.pretty $ "AssemblyError: " ++ show e
  pretty (BlockOverlappingRedirection cb) =
    PD.pretty "BlockOverlappingRedirection:" PD.<+> PD.pretty cb
  pretty (OverlayBlockNotContained cb) =
    PD.pretty "OverlayBlockNotContained:" PD.<+> PD.pretty cb

instance C.Exception BlockAssemblyException

-- | Given a list of basic blocks and the original text section, create two new
-- text sections.  The first contains all of the basic blocks that are laid out
-- in the original text section address range; byte ranges not covered by the
-- new blocks are copied from the original text section.  The second contains
-- all of the other blocks, which are assumed to be contiguous.
--
-- This function assumes that the extra contiguous blocks are at higher
-- addresses than the original text section.
assembleBlocks :: (C.MonadThrow m, InstructionConstraints i a, MM.MemWidth w)
               => ISA i a w
               -> MM.MemSegmentOff w
               -- ^ The address of the start of the text section
               -> B.ByteString
               -- ^ The original text section contents
               -> RelAddress w
               -- ^ The address to start laying out extra blocks at
               -> (forall m' . (C.MonadThrow m') => i () -> m' B.ByteString)
               -- ^ A function to assemble a single instruction to bytes
               -> [ConcreteBlock i w]
               -> m (B.ByteString, B.ByteString)
assembleBlocks isa textSecStart origTextBytes extraAddr assemble blocks = do
  s1 <- St.execStateT (unA assembleDriver) s0
  return (fromBuilder (asTextSection s1), fromBuilder (asExtraText s1))
  where
    absStartAddr = relFromSegmentOff textSecStart
    s0 = AssembleState { asTextStart = absStartAddr
                       , asTextAddr = absStartAddr
                       , asTextSection = mempty
                       , asOrigTextBytes = origTextBytes
                       , asExtraStart = extraAddr
                       , asExtraAddr = extraAddr
                       , asExtraText = mempty
                       , asAssemble = assemble
                       , asBlocks = L.sortOn basicBlockAddress blocks
                       , asISA = isa
                       }

assembleDriver :: (C.MonadThrow m, InstructionConstraints i a, MM.MemWidth w) => Assembler i a w m ()
assembleDriver = do
  mb <- takeNextBlock
  case mb of
    Nothing -> return ()
    Just b -> do
      extraStart <- St.gets asExtraStart
      case basicBlockAddress b < extraStart of
        True -> assembleAsText b
        False -> assembleAsExtra b
      assembleDriver

-- | Code in the extra section never overlaps, so we can just perform some basic
-- consistency check sand then append it.
assembleAsExtra :: (C.MonadThrow m, InstructionConstraints i a, MM.MemWidth w)
                => ConcreteBlock i w
                -> Assembler i a w m ()
assembleAsExtra b = do
  nextExtraAddr <- St.gets asExtraAddr
  unless (nextExtraAddr == basicBlockAddress b) $ do
    C.throwM (DiscontiguousBlocks b)
  bytes <- assembleBlock b
  let bsize = B.length bytes
  St.modify' $ \s -> s { asExtraAddr = asExtraAddr s `addressAddOffset` fromIntegral bsize
                       , asExtraText = asExtraText s <> B.byteString bytes
                       }

-- | A block in the text section may be overlapped by some number of blocks
-- following it.  In that case, we need to include the non-overlapped prefix of
-- this block and then copy in the overlapping blocks as necessary.
--
-- If there are overlapping blocks, they should be contiguous (but there may be
-- some slack space after the last overlapping block where no other blocks could
-- fit).
assembleAsText :: (C.MonadThrow m, InstructionConstraints i a, MM.MemWidth w)
               => ConcreteBlock i w
               -> Assembler i a w m ()
assembleAsText b = do
  padToBlockStart b
  -- Now look ahead to see if we have any blocks completely overlapping this -
  -- write a function that returns them all (and asserts that none extend past
  -- the end of the block).
  --
  -- Lay out the prefix (assert that there is enough space for the redirecting
  -- jump), then lay out the embedded blocks.  Then fill the suffix with traps.
  overlapping <- lookupOverlappingBlocks b

  checkedOverlappingAssemble b overlapping

-- | Assemble a block with its overlapping blocks into the text section.
--
-- The block should start at the current pointer into the text section.  The
-- overlapping blocks (if any) must start immediately after the redirecting
-- jump.  The overlapping blocks must be contained and contiguous.
--
-- There may be space after the last block that will be filled with traps.
checkedOverlappingAssemble :: (C.MonadThrow m, MM.MemWidth w)
                           => ConcreteBlock i w
                           -> [ConcreteBlock i w]
                           -> Assembler i a w m ()
checkedOverlappingAssemble b overlays = do
  baseBytes <- assembleBlock b
  (prefixSize, suffixSize) <- computeBaseBlockBytes b overlays
  assert (prefixSize <= B.length baseBytes) (return ())
  let prefixBytes = B.take prefixSize baseBytes
  assert (B.length prefixBytes == prefixSize) (return ())
  appendTextBytes prefixBytes

  -- Splice in all of the overlays
  overlayByteCounts <- T.forM overlays $ \overlay -> do
    curTextAddr <- St.gets asTextAddr
    assert (curTextAddr == basicBlockAddress overlay) (return ())
    overlayBytes <- assembleBlock overlay
    appendTextBytes overlayBytes
    return (B.length overlayBytes)

  -- Construct the suffix and splice it in
  --
  -- FIXME: Note that we are splicing in byte sequences of odd lengths, so we
  -- might end up slicing a trap instruction in half to create a new instruction
  -- that isn't what we intended.
  --
  -- For x86_64, this isn't a problem because traps are one byte.  Most other
  -- architectures have fixed-length instructions, so it might not matter.
  -- Thumb could be a problem, as some instructions are four bytes, while others
  -- are two.  We need to audit that.
  let overlayBytes = sum overlayByteCounts
  assert (B.length baseBytes == prefixSize + overlayBytes + suffixSize) (return ())
  let suffixBytes = B.drop (prefixSize + overlayBytes) baseBytes
  appendTextBytes suffixBytes

-- | Compute the prefix bytes of the concrete block that must be preserved and
-- the suffix bytes not occupied by overlaid blocks.
--
-- The prefix is the number of bytes required for the redirection jump.
--
-- The suffix is the number of bytes not occupied by overlay blocks, which could
-- be the rest of the bytes in the block.  The suffix bytes are copied from the
-- base block (and filled with traps).
computeBaseBlockBytes :: (Monad m) => ConcreteBlock i w -> [ConcreteBlock i w] -> Assembler i a w m (Int, Int)
computeBaseBlockBytes b overlays = do
  isa <- St.gets asISA
  let fakeJump = isaMakeRelativeJumpTo isa (basicBlockAddress b) (basicBlockAddress b)
      jumpSize = sum (map (isaInstructionSize isa) fakeJump)
      overlaySizes = sum (map (fromIntegral . concreteBlockSize isa) overlays)
      blockSize = concreteBlockSize isa b
  let prefix = min (fromIntegral jumpSize) (fromIntegral blockSize)
  let suffix = max 0 (fromIntegral blockSize - (overlaySizes + prefix))
  return (prefix, suffix)

appendTextBytes :: (MM.MemWidth w, Monad m) => B.ByteString -> Assembler i a w m ()
appendTextBytes bs = do
  St.modify' $ \s -> s { asTextSection = asTextSection s <> B.byteString bs
                       , asTextAddr = asTextAddr s `addressAddOffset` fromIntegral (B.length bs)
                       }

-- | Look up all of the blocks overlapping the given block.
--
-- If any of the overlapping blocks are not completely contained within the
-- input block, raises an error.
--
-- As a side effect, removes the overlapping blocks from the list of blocks to
-- be assembled.
lookupOverlappingBlocks :: forall i a w m
                         . (C.MonadThrow m, InstructionConstraints i a, MM.MemWidth w)
                        => ConcreteBlock i w
                        -> Assembler i a w m [ConcreteBlock i w]
lookupOverlappingBlocks b = do
  isa <- St.gets asISA
  let dummyJump = isaMakeRelativeJumpTo isa (basicBlockAddress b) (basicBlockAddress b)
      jumpSize = sum (map (isaInstructionSize isa) dummyJump)
      blockSize = concreteBlockSize isa b
      blockEnd = basicBlockAddress b `addressAddOffset` fromIntegral blockSize
  go isa blockEnd (basicBlockAddress b `addressAddOffset` fromIntegral jumpSize)
  where
    go :: ISA i a w -> RelAddress w -> RelAddress w -> Assembler i a w m [ConcreteBlock i w]
    go isa blockEnd nextAllowableAddress = do
      mb' <- takeNextBlock
      case mb' of
        Nothing -> return []
        Just b' -> do
          let bsize = fromIntegral (concreteBlockSize isa b')
          case () of
            _ | basicBlockAddress b' >= blockEnd -> do
                  -- If the next block comes after the current block, just put
                  -- it back and return
                  St.modify' $ \s -> s { asBlocks = b' : asBlocks s }
                  return []
              | basicBlockAddress b' < nextAllowableAddress -> do
                  -- We check this case second in case the block is shorter than
                  -- a redirection jump; in that case, the next block will
                  -- appear to be overlapping when really there is no
                  -- redirection jump at all.
                  C.throwM (BlockOverlappingRedirection b')
              | basicBlockAddress b' > nextAllowableAddress -> do
                  C.throwM (DiscontiguousBlocks b')
              | (basicBlockAddress b' `addressAddOffset` bsize) > blockEnd ->
                  C.throwM (OverlayBlockNotContained b')
              | otherwise -> do
                  (b':) <$> go isa blockEnd (nextAllowableAddress `addressAddOffset` bsize)

-- | If the given block doesn't start at the next expected address in the text
-- section, pull bytes from the original text section to pad it out until we
-- have alignment.
--
-- This covers cases where the previous block was followed by data (that was not
-- recognized as instructions by the analysis).  We have to preserve such data.
padToBlockStart :: (Monad m, MM.MemWidth w) => ConcreteBlock i w -> Assembler i a w m ()
padToBlockStart b = do
  nextAddr <- St.gets asTextAddr
  assert (nextAddr <= basicBlockAddress b) (return ())
  case nextAddr == basicBlockAddress b of
    True -> return ()
    False -> do
      origTextBytes <- St.gets asOrigTextBytes
      textStart <- St.gets asTextStart
      let gapSize = fromIntegral (basicBlockAddress b `addressDiff` nextAddr)
          idx = fromIntegral (nextAddr `addressDiff` textStart)
          gapBytes = B.take gapSize (B.drop idx origTextBytes)
      assert (B.length gapBytes == gapSize) (return ())
      St.modify' $ \s -> s { asTextAddr = asTextAddr s `addressAddOffset` fromIntegral gapSize
                           , asTextSection = asTextSection s <> B.byteString gapBytes
                           }

assembleBlock :: (C.MonadThrow m) => ConcreteBlock i w -> Assembler i a w m (B.ByteString)
assembleBlock b = do
  assembler <- St.gets asAssemble
  case mapM assembler (basicBlockInstructions b) of
    Left err -> C.throwM (AssemblyError err)
    Right strs -> return (mconcat strs)

takeNextBlock :: (Monad m) => Assembler i a w m (Maybe (ConcreteBlock i w))
takeNextBlock = do
  bs <- St.gets asBlocks
  case bs of
    [] -> return Nothing
    (b:rest) -> do
      St.modify' $ \s -> s { asBlocks = rest }
      return (Just b)

newtype Assembler i a w m a' = Assembler { unA :: St.StateT (AssembleState i a w) m a' }
                          deriving ( Functor,
                                     Applicative,
                                     Monad,
                                     C.MonadThrow,
                                     St.MonadState (AssembleState i a w) )

data AssembleState i a w =
  AssembleState { asTextStart :: RelAddress w
                -- ^ The starting address of the text section
                , asTextAddr :: !(RelAddress w)
                -- ^ The next address to fill in the text section builder
                , asTextSection :: !B.Builder
                -- ^ The text section we are building up out of new blocks and
                -- data pulled from the original text section
                , asExtraStart :: RelAddress w
                -- ^ The start of the extra text section
                , asExtraAddr :: !(RelAddress w)
                -- ^ The address for blocks that end up in the extra section.
                -- We keep this around to perform consistency checks (i.e., to
                -- ensure that the blocks are really contiguous).
                , asExtraText :: !B.Builder
                -- ^ The section we are building up of new blocks that are
                -- expected to be contiguous.
                , asAssemble :: i () -> Either C.SomeException B.ByteString
                -- ^ The assembler to turn instructions into bytes
                , asBlocks :: [ConcreteBlock i w]
                -- ^ The blocks remaining to process; these must be ordered by address
                , asOrigTextBytes :: B.ByteString
                -- ^ The original bytes of the text section, used to extract
                -- bits that are not covered by basic blocks
                , asISA :: ISA i a w
                }

fromBuilder :: B.Builder -> B.ByteString
fromBuilder = L.toStrict . B.toLazyByteString
