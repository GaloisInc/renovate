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

import qualified GHC.Err.Located as L
import           Control.Applicative
import           Control.Exception ( assert )
import qualified Control.Lens as L
import           Control.Monad ( when, unless )
import qualified Control.Monad.Catch as C
import qualified Control.Monad.State.Strict as St
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import qualified Data.List as L
import           Data.Monoid
import qualified Data.Text.Prettyprint.Doc as PD

import           Prelude

import qualified Data.Macaw.CFG as MM

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA

data BlockAssemblyException where
  -- A discontiguous block was starting with the given concrete block
  DiscontiguousBlocks         :: forall arch
                               . (InstructionConstraints arch)
                              => ConcreteBlock arch
                              -> ConcreteAddress arch
                              -> BlockAssemblyException

  UnexpectedMemoryContents    :: forall w
                               . (MM.MemWidth w)
                              => MM.MemSegmentOff w -> BlockAssemblyException

  AssemblyError               :: C.SomeException -> BlockAssemblyException

  BlockOverlappingRedirection :: forall arch
                               . (InstructionConstraints arch)
                              => ConcreteBlock arch -> BlockAssemblyException

  OverlayBlockNotContained    :: forall arch
                               . (InstructionConstraints arch)
                              => ConcreteBlock arch -> BlockAssemblyException

deriving instance Show BlockAssemblyException

instance PD.Pretty BlockAssemblyException where
  pretty (DiscontiguousBlocks cb nextAddr) =
    PD.pretty "DiscontiguousBlocks:" PD.<+> PD.pretty cb PD.<+> PD.pretty "/" PD.<+> PD.pretty nextAddr
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
--
-- This function assumes (and asserts) that, if any blocks are overlapped, the
-- overlap is complete and the overlapped blocks share an end address.
assembleBlocks :: (L.HasCallStack, C.MonadThrow m, InstructionConstraints arch)
               => MM.Memory (MM.ArchAddrWidth arch)
               -> ISA arch
               -> ConcreteAddress arch
               -- ^ The address of the start of the text section
               -> ConcreteAddress arch
               -- ^ The address of the end of the text section
               -> B.ByteString
               -- ^ The original text section contents
               -> ConcreteAddress arch
               -- ^ The address to start laying out extra blocks at
               -> (forall m' . (C.MonadThrow m') => Instruction arch () -> m' B.ByteString)
               -- ^ A function to assemble a single instruction to bytes
               -> [ConcreteBlock arch]
               -> [(ConcreteAddress arch, B.ByteString)]
               -> m (B.ByteString, B.ByteString)
assembleBlocks mem isa absStartAddr absEndAddr origTextBytes extraAddr assemble blocks injectedCode = do
  s1 <- St.execStateT (unA assembleDriver) s0
  return (fromBuilder (asTextSection s1), fromBuilder (asExtraText s1))
  where
    s0 = AssembleState { asTextStart        = absStartAddr
                       , asTextEnd          = absEndAddr
                       , asTextAddr         = absStartAddr
                       , asTextSection      = mempty
                       , asOrigTextBytes    = origTextBytes
                       , asExtraStart       = extraAddr
                       , asExtraAddr        = extraAddr
                       , asExtraText        = mempty
                       , asAssemble         = assemble
                       , _asOrigBlocks      = L.sortOn basicBlockAddress filteredBlocks
                       , _asAllocatedBlocks = L.sortOn basicBlockAddress allocatedBlocks
                       , asISA              = isa
                       , asMemory           = mem
                       , asInstructionConstraints = id
                       }
    -- Split the inputs block list into 2 lists. One for blocks that fit in the
    -- original address space and one for the newly allocated blocks.
    (origBlocks, allocatedBlocks) = foldr go ([],[]) blocks
    filteredBlocks = filter inText origBlocks
    inText b = absoluteAddress absStartAddr <= absoluteAddress (basicBlockAddress b) &&
               absoluteAddress (basicBlockAddress b) < absoluteAddress absEndAddr
    go b (origAcc, allocatedAcc) = case MM.resolveAbsoluteAddr mem (absoluteAddress (basicBlockAddress b)) of
      Nothing -> (origAcc, b:allocatedAcc)
      Just _  -> (b:origAcc, allocatedAcc)

-- | Process all the input blocks. First, look at each block that will be in the
-- original address space of the binary. Then look at each block that will be
-- newly allocated.
assembleDriver :: forall m arch . (C.MonadThrow m, InstructionConstraints arch) => Assembler arch m ()
assembleDriver = do
  mb <- takeNextOrigBlock
  case mb of
    Just b -> do
      assembleAsTextOrExtra b
      isLast <- isLastOrigBlock
      -- If this is the last block in the original address space but does not
      -- fill out the address space then we need to use the rest of the
      -- original byte sequence to pad out the original address space.
      when isLast padLastBlock
      assembleDriver
    Nothing -> do
      mb' <- takeNextAllocatedBlock
      case mb' of
        Nothing -> return ()
        Just b  -> do
          assembleAsTextOrExtra b
          assembleDriver
  where
  assembleAsTextOrExtra :: ConcreteBlock arch -> Assembler arch m ()
  assembleAsTextOrExtra b = do
    extraStart <- St.gets asExtraStart
    case basicBlockAddress b < extraStart of
      True  -> assembleAsText b
      False -> assembleAsExtra b

-- | Code in the extra section never overlaps, so we can just perform some basic
-- consistency check sand then append it.
assembleAsExtra :: (L.HasCallStack, C.MonadThrow m, InstructionConstraints arch)
                => ConcreteBlock arch
                -> Assembler arch m ()
assembleAsExtra b = do
  padToBlockStartExtra b
  nextExtraAddr <- St.gets asExtraAddr
  -- traceM $ "nextExtraAddr = " ++ show nextExtraAddr
  -- traceM $ "b = " ++ show (basicBlockAddress b)
  unless (nextExtraAddr == basicBlockAddress b) $ do
    C.throwM (DiscontiguousBlocks b nextExtraAddr)
  bytes <- assembleBlock b
  let bsize = B.length bytes
  St.modify' $ \s -> s { asExtraAddr = asExtraAddr s `addressAddOffset` fromIntegral bsize
                       , asExtraText = asExtraText s <> B.byteString bytes
                       }

assertM :: (L.HasCallStack, Applicative m) => Bool -> m ()
assertM b = assert b (pure ())

-- | A block in the text section may be overlapped by some number of blocks
-- following it.  In that case, we need to include the non-overlapped prefix of
-- this block and then copy in the overlapping blocks as necessary.
--
-- If there are overlapping blocks, they should be contiguous (but there may be
-- some slack space after the last overlapping block where no other blocks could
-- fit).
assembleAsText :: (L.HasCallStack, C.MonadThrow m, InstructionConstraints arch)
               => ConcreteBlock arch
               -> Assembler arch m ()
assembleAsText b = do
  -- If there is a gap between the next layout address and the start of this
  -- block, fill the gap with bytes from the original text section.
  padToBlockStart b

  -- Look up any blocks that overlap the current block and return them (while
  -- removing them from the assembly queue).
  overlapping <- lookupOverlappingBlocks b

  -- Ensure that all of the restrictions we have in place are obeyed and then
  -- add the block to the text section.
  checkedOverlappingAssemble b overlapping

-- | Compute the end address of a basic block
blockEndAddress :: (MM.MemWidth (MM.ArchAddrWidth arch))
                => ISA arch
                -> ConcreteBlock arch
                -> ConcreteAddress arch
blockEndAddress isa b =
  basicBlockAddress b `addressAddOffset` fromIntegral (concreteBlockSize isa b)

-- | Assemble a block with its overlapping blocks into the text section.
--
-- The block should start at the current pointer into the text section.  The
-- overlapping blocks (if any) must start immediately after the redirecting
-- jump.  The overlapping blocks must be contained and contiguous.
--
-- There may be space after the last block that will be filled with traps.
checkedOverlappingAssemble :: (L.HasCallStack, C.MonadThrow m, MM.MemWidth (MM.ArchAddrWidth arch))
                           => ConcreteBlock arch
                           -> [ConcreteBlock arch]
                           -> Assembler arch m ()
checkedOverlappingAssemble b overlays = withInstructionConstraints $ do
  isa <- St.gets asISA
  let blockEnd = blockEndAddress isa b
      reversedInstructions = reverse (basicBlockInstructions b)
  -- assert that the overlays are all completely contained in the first block
  F.forM_ overlays $ \overlay -> do
    let overlayEnd = blockEndAddress isa overlay
    assertM (basicBlockAddress overlay > basicBlockAddress b)
    assertM (overlayEnd == blockEnd)
    assertM (reverse (basicBlockInstructions overlay) `L.isPrefixOf` reversedInstructions)

  baseBytes <- assembleBlock b
  appendTextBytes baseBytes

-- | Append bytes to the text section, updating the next available layout
-- address
appendTextBytes :: (MM.MemWidth (MM.ArchAddrWidth arch), Monad m) => B.ByteString -> Assembler arch m ()
appendTextBytes bs = do
  St.modify' $ \s -> s { asTextSection = asTextSection s <> B.byteString bs
                       , asTextAddr = asTextAddr s `addressAddOffset` fromIntegral (B.length bs)
                       }

-- | Look up (and return) all of the blocks overlapping the given block.
--
-- If any of the overlapping blocks are not completely contained within the
-- input block, raises an error.
--
-- As a side effect, removes the overlapping blocks from the list of blocks to
-- be assembled.
lookupOverlappingBlocks :: forall arch m
                         . (L.HasCallStack, C.MonadThrow m, InstructionConstraints arch)
                        => ConcreteBlock arch
                        -> Assembler arch m [ConcreteBlock arch]
lookupOverlappingBlocks b = do
  isa <- St.gets asISA
  let blockSize = concreteBlockSize isa b
      blockEnd  = basicBlockAddress b `addressAddOffset` fromIntegral blockSize
  go isa blockEnd
  where
    -- Look up the next block and see if it overlaps the current block @b@.
    -- Stop looking when we find a block that clearly does not overlap.
    go :: ISA arch -> ConcreteAddress arch -> Assembler arch m [ConcreteBlock arch]
    go isa blockEnd = do
      mb' <- takeNextOrigBlock
      case mb' of
        Nothing -> return []
        Just b' -> do
          let bsize = fromIntegral (concreteBlockSize isa b')
          case () of
            _ | basicBlockAddress b' >= blockEnd -> do
                  -- If the next block comes after the current block, just put
                  -- it back and return
                  asOrigBlocks L.%= (b':)
                  return []
              | (basicBlockAddress b' `addressAddOffset` bsize) > blockEnd ->
                -- We want to assert that, if any blocks overlap, they all end
                -- at the same instruction.  If that isn't the case, it kind of
                -- indicates an inconsistent set of blocks coming from macaw or
                -- the rewriter.
                  C.throwM (OverlayBlockNotContained b')
              | otherwise -> do
                  (b':) <$> go isa blockEnd

-- | If the given block doesn't start at the next expected address in the text
-- section, pull bytes from the original text section to pad it out until we
-- have alignment.
--
-- This covers cases where the previous block was followed by data (that was not
-- recognized as instructions by the analysis).  We have to preserve such data.
padToBlockStart :: (L.HasCallStack, Monad m, MM.MemWidth (MM.ArchAddrWidth arch))
                => ConcreteBlock arch
                -> Assembler arch m ()
padToBlockStart b = do
  nextAddr  <- St.gets asTextAddr
  -- startAddr <- St.gets asTextStart
  -- endAddr   <- St.gets asTextEnd
  -- Only do padding when the basic block is in the .text
  -- when (startAddr <= basicBlockAddress b && basicBlockAddress b <= endAddr) $ do
  assertM (nextAddr <= basicBlockAddress b)
  case nextAddr == basicBlockAddress b of
    True -> return ()
    False -> do
      -- traceM "generating padding"
      -- traceM $ "startAddr = " ++ show startAddr
      -- traceM $ "nextAddr  = " ++ show nextAddr
      -- traceM $ "b         = " ++ show (basicBlockAddress b)
      -- traceM $ "endAddr   = " ++ show endAddr
      origTextBytes <- St.gets asOrigTextBytes
      textStart     <- St.gets asTextStart
      let gapSize  = fromIntegral (basicBlockAddress b `addressDiff` nextAddr)
          idx      = fromIntegral (nextAddr `addressDiff` textStart)
          gapBytes = B.take gapSize (B.drop idx origTextBytes)
      -- traceM $ "gapSize = " ++ show gapSize
      -- traceM ""
      assertM (B.length gapBytes == gapSize)
      St.modify' $ \s -> s { asTextAddr    = asTextAddr s `addressAddOffset` fromIntegral gapSize
                           , asTextSection = asTextSection s <> B.byteString gapBytes
                           }

padToBlockStartExtra :: (L.HasCallStack, Monad m, MM.MemWidth (MM.ArchAddrWidth arch))
                     => ConcreteBlock arch
                     -> Assembler arch m ()
padToBlockStartExtra b = do
  nextExtraAddr <- St.gets asExtraAddr
  assertM (nextExtraAddr <= basicBlockAddress b)
  case nextExtraAddr == basicBlockAddress b of
    True -> return ()
    False -> do
      let gapSize  = fromIntegral (basicBlockAddress b `addressDiff` nextExtraAddr)
          gapBytes = B.replicate gapSize 0
      assertM (B.length gapBytes == gapSize)
      St.modify' $ \s -> s { asExtraAddr = asExtraAddr s `addressAddOffset` fromIntegral gapSize
                           , asExtraText = asExtraText s <> B.byteString gapBytes
                           }

-- | Looks for a gap after the current block (assumes, current block is last of
-- the blocks for the original address space) and fills that gap with the bytes
-- from the original program text.
-- TODO: can this logic benefit from knowing the textEnd?
padLastBlock :: (Monad m, MM.MemWidth (MM.ArchAddrWidth arch)) => Assembler arch m ()
padLastBlock = do
  origTextBytes <- St.gets asOrigTextBytes
  nextAddr      <- St.gets asTextAddr
  textStart     <- St.gets asTextStart
  let idx          = fromIntegral (nextAddr `addressDiff` textStart)
      leftOversLen = B.length origTextBytes - idx
      leftOvers    = B.take leftOversLen (B.drop idx origTextBytes)
  -- traceM $ "padLastBlock = " ++ show (B.length origTextBytes - idx)
  if leftOversLen > 0
     then do
       St.modify' $ \s -> s { asTextAddr    = asTextAddr s `addressAddOffset` fromIntegral leftOversLen
                            , asTextSection = asTextSection s <> B.byteString leftOvers
                            }
     else return ()

assembleBlock :: (L.HasCallStack, C.MonadThrow m) => ConcreteBlock arch -> Assembler arch m (B.ByteString)
assembleBlock b = do
  assembler <- St.gets asAssemble
  case mapM assembler (basicBlockInstructions b) of
    Left err -> C.throwM (AssemblyError err)
    Right strs -> return (mconcat strs)

-- | Helper function for taking the next block.
takeNextBlockWith :: (Monad m)
                  => L.Lens' (AssembleState arch) [ConcreteBlock arch]
                  -> Assembler arch m (Maybe (ConcreteBlock arch))
takeNextBlockWith f = do
  bs <- L.use f
  case bs of
    [] -> return Nothing
    (b:rest) -> do
      f L..= rest
      return $! Just b

-- | Grabs the next block from the orginal block set (original in the sense that
-- these blocks have addresses in the original address space).
takeNextOrigBlock :: (Monad m) => Assembler arch m (Maybe (ConcreteBlock arch))
takeNextOrigBlock = takeNextBlockWith asOrigBlocks

-- | Grabs the next block from the allocated block set (allocated in the sense that
-- these blocks DO NOT have addresses in the original address space).
takeNextAllocatedBlock :: (Monad m) => Assembler arch m (Maybe (ConcreteBlock arch))
takeNextAllocatedBlock = takeNextBlockWith asAllocatedBlocks

-- | Checks if the the orginal block list is exhausted.
-- Note: This will return true when the current block is from the allocated set.
isLastOrigBlock :: (Monad m) => Assembler arch m Bool
isLastOrigBlock = do
  bs <- L.use asOrigBlocks
  return $! null bs

newtype Assembler arch m a = Assembler { unA :: St.StateT (AssembleState arch) m a }
                            deriving ( Functor,
                                       Applicative,
                                       Monad,
                                       C.MonadThrow,
                                       St.MonadState (AssembleState arch) )

data AssembleState arch =
  AssembleState { asTextStart :: ConcreteAddress arch
                -- ^ The starting address of the text section
                , asTextEnd   :: ConcreteAddress arch
                -- ^ The ending address of the text section
                , asTextAddr :: !(ConcreteAddress arch)
                -- ^ The next address to fill in the text section builder
                , asTextSection :: !B.Builder
                -- ^ The text section we are building up out of new blocks and
                -- data pulled from the original text section
                , asExtraStart :: ConcreteAddress arch
                -- ^ The start of the extra text section
                , asExtraAddr :: !(ConcreteAddress arch)
                -- ^ The address for blocks that end up in the extra section.
                -- We keep this around to perform consistency checks (i.e., to
                -- ensure that the blocks are really contiguous).
                , asExtraText :: !B.Builder
                -- ^ The section we are building up of new blocks that are
                -- expected to be contiguous.
                , asAssemble :: Instruction arch () -> Either C.SomeException B.ByteString
                -- ^ The assembler to turn instructions into bytes
                , _asOrigBlocks :: [ConcreteBlock arch]
                -- ^ The blocks remaining to process. These must be ordered by
                -- address and will go into the original address space of the
                -- binary.
                , _asAllocatedBlocks :: [ConcreteBlock arch]
                -- ^ The blocks remaining to process. These must be ordered by
                -- address but their addresses are outside the range of the
                -- original binary and must be allocated in a new part of the
                -- elf file.
                , asOrigTextBytes :: B.ByteString
                -- ^ The original bytes of the text section, used to extract
                -- bits that are not covered by basic blocks
                , asISA :: ISA arch
                , asMemory :: MM.Memory (MM.ArchAddrWidth arch)
                -- ^ The macaw memory object
                , asInstructionConstraints :: forall a. (InstructionConstraints arch => a) -> a
                }

asOrigBlocks :: L.Lens' (AssembleState arch) [ConcreteBlock arch]
asOrigBlocks = L.lens _asOrigBlocks (\as bs -> as { _asOrigBlocks = bs })

asAllocatedBlocks :: L.Lens' (AssembleState arch) [ConcreteBlock arch]
asAllocatedBlocks = L.lens _asAllocatedBlocks (\as bs -> as { _asAllocatedBlocks = bs })

withInstructionConstraints :: Monad m => (InstructionConstraints arch => Assembler arch m a) -> Assembler arch m a
withInstructionConstraints act = do
  as <- St.get
  asInstructionConstraints as act

fromBuilder :: B.Builder -> B.ByteString
fromBuilder = LBS.toStrict . B.toLazyByteString
