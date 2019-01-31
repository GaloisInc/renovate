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
import           Control.Monad ( unless )
import qualified Control.Monad.Catch as C
import qualified Control.Monad.State.Strict as St
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import qualified Data.List as L
import           Data.Monoid
import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Word ( Word64 )

import           Prelude

import qualified Data.Macaw.CFG as MM

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA

data BlockAssemblyException where
  -- A discontiguous block was starting with the given concrete block
  DiscontiguousBlocks         :: forall arch
                               . (InstructionConstraints arch)
                              => Chunk arch
                              -> ConcreteAddress arch
                              -> BlockAssemblyException

  UnexpectedMemoryContents    :: forall w
                               . (MM.MemWidth w)
                              => MM.MemSegmentOff w -> BlockAssemblyException

  AssemblyError               :: C.SomeException -> BlockAssemblyException

  BlockOverlappingRedirection :: forall arch
                               . (InstructionConstraints arch)
                              => Chunk arch -> BlockAssemblyException

  OverlayBlockNotContained    :: forall arch
                               . (InstructionConstraints arch)
                              => Chunk arch -> Chunk arch -> BlockAssemblyException

deriving instance Show BlockAssemblyException

instance PD.Pretty BlockAssemblyException where
  pretty (DiscontiguousBlocks cb nextAddr) =
    PD.pretty "DiscontiguousBlocks:" PD.<+> PD.pretty cb PD.<+> PD.pretty "/" PD.<+> PD.pretty nextAddr
  pretty (UnexpectedMemoryContents seg) =
    PD.pretty "UnexpectedMemoryContents:" PD.<+> PD.pretty (show seg)
  pretty (AssemblyError e) = PD.pretty $ "AssemblyError: " ++ show e
  pretty (BlockOverlappingRedirection cb) =
    PD.pretty "BlockOverlappingRedirection:" PD.<+> PD.pretty cb
  pretty (OverlayBlockNotContained orig overlay) =
    PD.vsep [ PD.pretty "OverlayBlockNotContained:"
            , PD.indent 2 (PD.pretty "Base block:")
            , PD.indent 4 (PD.pretty orig)
            , PD.indent 2 (PD.pretty "Overlay block:")
            , PD.indent 4 (PD.pretty overlay)
            ]

instance (InstructionConstraints arch) => PD.Pretty (Chunk arch) where
  pretty (BlockChunk b) = PD.pretty b
  pretty (RawChunk addr bs) = PD.pretty addr PD.<> PD.pretty "@" PD.<> PD.pretty (B.length bs)

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
               -> [(SymbolicAddress arch, ConcreteAddress arch, B.ByteString)]
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
                       , _asOrigChunks      = L.sortOn chunkAddress filteredBlocks
                       , _asAllocatedChunks = L.sortOn chunkAddress allocatedBlocks
                       , asISA              = isa
                       , asMemory           = mem
                       , asInstructionConstraints = id
                       }
    -- Split the inputs block list into 2 lists. One for blocks that fit in the
    -- original address space and one for the newly allocated blocks.
    --
    -- We mix basic blocks and raw injected code using the 'Chunk' abstraction
    -- below.  Most of the code here doesn't really need to know anything
    -- besides the address and size of each chunk (and how to turn a chunk into
    -- a bytestring).
    allCode = map BlockChunk blocks ++ [ RawChunk addr bytes | (_, addr, bytes) <- injectedCode ]
    (origBlocks, allocatedBlocks) = foldr go ([],[]) allCode
    filteredBlocks = filter inText origBlocks
    inText c = absoluteAddress absStartAddr <= absoluteAddress (chunkAddress c) &&
               absoluteAddress (chunkAddress c) < absoluteAddress absEndAddr
    -- NOTE: This test (using resolveAbsoluteAddr) is depending on the fact that
    -- resolveAbsoluteAddr will fail for any address that is not in the macaw
    -- Memory (which our new code will not be)
    go c (origAcc, allocatedAcc) = case MM.resolveAbsoluteAddr mem (absoluteAddress (chunkAddress c)) of
      Nothing -> (origAcc, c:allocatedAcc)
      Just _  -> (c:origAcc, allocatedAcc)

-- | Process all the input blocks. First, look at each block that will be in the
-- original address space of the binary. Then look at each block that will be
-- newly allocated.
assembleDriver :: forall m arch . (C.MonadThrow m, InstructionConstraints arch) => Assembler arch m ()
assembleDriver = do
  textStart <- St.gets asTextStart
  textEnd <- St.gets asTextEnd

  let inText b = chunkAddress b >= textStart && chunkAddress b < textEnd

      loopOrig = do
        mb <- takeNextOrigBlock
        case mb of
          Just b -> do
            assertM (inText b)
            assembleAsText b
            loopOrig
          Nothing -> do
            -- If this is the last block in the original address space but does not
            -- fill out the address space then we need to use the rest of the
            -- original byte sequence to pad out the original address space.
            padLastBlock
            loopAllocated

      loopAllocated = do
        mb <- takeNextAllocatedChunk
        case mb of
          Just b -> do
            assertM (not (inText b))
            assembleAsExtra b
            loopAllocated
          Nothing -> return ()

  loopOrig

  -- double check that nobody else put something on the "to-assemble" list
  -- behind our backs
  L.use asOrigChunks      >>= assertM . null
  L.use asAllocatedChunks >>= assertM . null

-- | Code in the extra section never overlaps, so we can just perform some basic
-- consistency check sand then append it.
assembleAsExtra :: (L.HasCallStack, C.MonadThrow m, InstructionConstraints arch)
                => Chunk arch
                -> Assembler arch m ()
assembleAsExtra b = do
  padToBlockStartExtra b
  nextExtraAddr <- St.gets asExtraAddr
  -- traceM $ "nextExtraAddr = " ++ show nextExtraAddr
  -- traceM $ "b = " ++ show (chunkAddress b)
  unless (nextExtraAddr == chunkAddress b) $ do
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
               => Chunk arch
               -> Assembler arch m ()
assembleAsText c = do
  -- If there is a gap between the next layout address and the start of this
  -- block, fill the gap with bytes from the original text section.
  padToBlockStart c

  -- Look up any blocks that overlap the current block and return them (while
  -- removing them from the assembly queue).
  overlapping <- lookupOverlappingBlocks c

  -- Ensure that all of the restrictions we have in place are obeyed and then
  -- add the block to the text section.
  checkedOverlappingAssemble c overlapping

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
                           => Chunk arch
                           -> [Chunk arch]
                           -> Assembler arch m ()
checkedOverlappingAssemble c overlays = do
  case c of
    RawChunk {} ->
      -- RawChunks represent new code that the caller has injected.  This code
      -- should *never* overlap any other code in the binary
      assertM (null overlays)
    BlockChunk b -> withInstructionConstraints $ do
      isa <- St.gets asISA
      let blockEnd = blockEndAddress isa b
      let reversedInstructions = reverse (basicBlockInstructions b)
      -- Assert that the overlays (i.e., blocks that overlay this current block
      -- that we are assembling) are all completely contained in the first block.
      --
      -- NOTE: We don't generate any overlapping blocks as part of the rewriting
      -- (if a block is to be redirected and its space is to be re-used, the
      -- original block is turned into a one-instruction block that starts with
      -- a redirecting jump).  Overlapping blocks should *only* come from the
      -- original binary.
      F.forM_ overlays $ \overlay -> do
        case overlay of
          RawChunk {} -> assertM False
          BlockChunk overlayBlock -> do
            let overlayEnd = blockEndAddress isa overlayBlock
            assertM (basicBlockAddress overlayBlock > basicBlockAddress b)
            assertM (overlayEnd == blockEnd)
            bytesB <- bytesFor b
            bytesOverlay <- bytesFor overlayBlock
            assertM (bytesOverlay `B.isSuffixOf` bytesB)
  bytes <- assembleBlock c
  appendTextBytes bytes

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
                        => Chunk arch
                        -> Assembler arch m [Chunk arch]
lookupOverlappingBlocks b = do
  isa <- St.gets asISA
  let blockSize = chunkSize isa b
      blockEnd  = chunkAddress b `addressAddOffset` fromIntegral blockSize
  go isa blockEnd
  where
    -- Look up the next block and see if it overlaps the current block @b@.
    -- Stop looking when we find a block that clearly does not overlap.
    go :: ISA arch -> ConcreteAddress arch -> Assembler arch m [Chunk arch]
    go isa blockEnd = do
      mb' <- takeNextOrigBlock
      case mb' of
        Nothing -> return []
        Just b' -> do
          let bsize = fromIntegral (chunkSize isa b')
          case () of
            _ | chunkAddress b' >= blockEnd -> do
                  -- If the next block comes after the current block, just put
                  -- it back and return
                  asOrigChunks L.%= (b':)
                  return []
              | (chunkAddress b' `addressAddOffset` bsize) > blockEnd ->
                -- We want to assert that, if any blocks overlap, they all end
                -- at the same instruction.  If that isn't the case, it kind of
                -- indicates an inconsistent set of blocks coming from macaw or
                -- the rewriter.
                  C.throwM (OverlayBlockNotContained b b')
              | otherwise -> do
                  (b':) <$> go isa blockEnd

-- | If the given block doesn't start at the next expected address in the text
-- section, pull bytes from the original text section to pad it out until we
-- have alignment.
--
-- This covers cases where the previous block was followed by data (that was not
-- recognized as instructions by the analysis).  We have to preserve such data.
padToBlockStart :: (L.HasCallStack, Monad m, MM.MemWidth (MM.ArchAddrWidth arch))
                => Chunk arch
                -> Assembler arch m ()
padToBlockStart b = do
  nextAddr  <- St.gets asTextAddr
  -- startAddr <- St.gets asTextStart
  -- endAddr   <- St.gets asTextEnd
  -- Only do padding when the basic block is in the .text
  -- when (startAddr <= basicBlockAddress b && basicBlockAddress b <= endAddr) $ do
  assertM (nextAddr <= chunkAddress b)
  case nextAddr == chunkAddress b of
    True -> return ()
    False -> do
      -- traceM "generating padding"
      -- traceM $ "startAddr = " ++ show startAddr
      -- traceM $ "nextAddr  = " ++ show nextAddr
      -- traceM $ "b         = " ++ show (basicBlockAddress b)
      -- traceM $ "endAddr   = " ++ show endAddr
      origTextBytes <- St.gets asOrigTextBytes
      textStart     <- St.gets asTextStart
      let gapSize  = fromIntegral (chunkAddress b `addressDiff` nextAddr)
          idx      = fromIntegral (nextAddr `addressDiff` textStart)
          gapBytes = B.take gapSize (B.drop idx origTextBytes)
      -- traceM $ "gapSize = " ++ show gapSize
      -- traceM ""
      assertM (B.length gapBytes == gapSize)
      St.modify' $ \s -> s { asTextAddr    = asTextAddr s `addressAddOffset` fromIntegral gapSize
                           , asTextSection = asTextSection s <> B.byteString gapBytes
                           }

padToBlockStartExtra :: (L.HasCallStack, Monad m, MM.MemWidth (MM.ArchAddrWidth arch))
                     => Chunk arch
                     -> Assembler arch m ()
padToBlockStartExtra b = do
  nextExtraAddr <- St.gets asExtraAddr
  assertM (nextExtraAddr <= chunkAddress b)
  case nextExtraAddr == chunkAddress b of
    True -> return ()
    False -> do
      let gapSize  = fromIntegral (chunkAddress b `addressDiff` nextExtraAddr)
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

bytesFor :: (C.MonadThrow m) => ConcreteBlock arch -> Assembler arch m B.ByteString
bytesFor b = do
  asm1 <- St.gets asAssemble
  case mapM asm1 (basicBlockInstructions b) of
    Left err -> C.throwM (AssemblyError err)
    Right strs -> return (mconcat strs)

assembleBlock :: (L.HasCallStack, C.MonadThrow m) => Chunk arch -> Assembler arch m (B.ByteString)
assembleBlock c =
  case c of
    BlockChunk b -> bytesFor b
    RawChunk _ b -> return b

-- | Helper function for taking the next block.
takeNextChunkWith :: (Monad m)
                  => L.Lens' (AssembleState arch) [Chunk arch]
                  -> Assembler arch m (Maybe (Chunk arch))
takeNextChunkWith f = do
  bs <- L.use f
  case bs of
    [] -> return Nothing
    (b:rest) -> do
      f L..= rest
      return $! Just b

-- | Grabs the next block from the orginal block set (original in the sense that
-- these blocks have addresses in the original address space).
takeNextOrigBlock :: (Monad m) => Assembler arch m (Maybe (Chunk arch))
takeNextOrigBlock = takeNextChunkWith asOrigChunks

-- | Grabs the next block from the allocated block set (allocated in the sense that
-- these blocks DO NOT have addresses in the original address space).
takeNextAllocatedChunk :: (Monad m) => Assembler arch m (Maybe (Chunk arch))
takeNextAllocatedChunk = takeNextChunkWith asAllocatedChunks

newtype Assembler arch m a = Assembler { unA :: St.StateT (AssembleState arch) m a }
                            deriving ( Functor,
                                       Applicative,
                                       Monad,
                                       C.MonadThrow,
                                       St.MonadState (AssembleState arch) )

data Chunk arch = BlockChunk (ConcreteBlock arch)
                | RawChunk (ConcreteAddress arch) B.ByteString

deriving instance (InstructionConstraints arch) => Show (Chunk arch)

chunkAddress :: Chunk arch -> ConcreteAddress arch
chunkAddress c =
  case c of
    BlockChunk b -> basicBlockAddress b
    RawChunk addr _ -> addr

chunkSize :: ISA arch -> Chunk arch -> Word64
chunkSize isa c =
  case c of
    BlockChunk b -> concreteBlockSize isa b
    RawChunk _ b -> fromIntegral (B.length b)

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
                , _asOrigChunks :: [Chunk arch]
                -- ^ The blocks remaining to process. These must be ordered by
                -- address and will go into the original address space of the
                -- binary.
                , _asAllocatedChunks :: [Chunk arch]
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

asOrigChunks :: L.Lens' (AssembleState arch) [Chunk arch]
asOrigChunks = L.lens _asOrigChunks (\as bs -> as { _asOrigChunks = bs })

asAllocatedChunks :: L.Lens' (AssembleState arch) [Chunk arch]
asAllocatedChunks = L.lens _asAllocatedChunks (\as bs -> as { _asAllocatedChunks = bs })

withInstructionConstraints :: Monad m => (InstructionConstraints arch => Assembler arch m a) -> Assembler arch m a
withInstructionConstraints act = do
  as <- St.get
  asInstructionConstraints as act

fromBuilder :: B.Builder -> B.ByteString
fromBuilder = LBS.toStrict . B.toLazyByteString
