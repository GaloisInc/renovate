{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Convert collections of basic blocks (specifically 'ConcreteBlock's) to
-- contiguous regions of bytes.
module Renovate.Assemble
( assembleBlocks
) where

import qualified Control.Lens as L
import           Control.Monad ( unless, when )
import qualified Control.Monad.Catch as C
import qualified Control.Monad.State.Strict as St
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
import qualified Data.List as L
import           Data.Semigroup ( sconcat )
import           GHC.Stack ( HasCallStack )

import           Prelude

import qualified Data.Macaw.CFG as MM

import           Renovate.Core.Address
import           Renovate.Core.BasicBlock
import qualified Renovate.Core.Chunk as RCC
import qualified Renovate.Core.Exception as RCE
import qualified Renovate.Core.Instruction as RCI
import           Renovate.ISA
import qualified Renovate.Panic as RP
import qualified Renovate.Redirect as RR
import qualified Renovate.Redirect.Concretize as RRC

instructionBlockChunk
  :: (SymbolicAddress arch, ConcreteAddress arch, RRC.InjectConcreteInstructions arch)
  -> RCC.Chunk arch
instructionBlockChunk (_symAddr, concAddr, RRC.InjectConcreteInstructions repr insns) =
  RCC.BlockChunk (concretizedBlock concAddr insns repr)

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
assembleBlocks :: (HasCallStack, C.MonadThrow m, MM.MemWidth (MM.ArchAddrWidth arch))
               => MM.Memory (MM.ArchAddrWidth arch)
               -> ISA arch
               -> (ConcreteAddress arch, ConcreteAddress arch)
               -- ^ The extents of the text section
               -> B.ByteString
               -- ^ The original text section contents
               -> ConcreteAddress arch
               -- ^ The address to start laying out extra blocks at
               -> (forall m' tp . (C.MonadThrow m') => RCI.Instruction arch tp () -> m' B.ByteString)
               -- ^ A function to assemble a single instruction to bytes
               -> RR.RedirectionResult arch
               -> m (B.ByteString, B.ByteString)
assembleBlocks mem isa (absStartAddr, absEndAddr) origTextBytes extraAddr assemble rr = do
  s1 <- St.execStateT (unA assembleDriver) s0
  return (fromBuilder (asTextSection s1), fromBuilder (asExtraText s1))
  where
    blocks = RR.rrRedirectedBlocks rr
    injectedCode = RR.rrInjectedBlocks rr
    injectedInsns = RR.rrInjectedInstructions rr
    s0 = AssembleState { asTextStart        = absStartAddr
                       , asTextEnd          = absEndAddr
                       , asTextAddr         = absStartAddr
                       , asTextSection      = mempty
                       , asOrigTextBytes    = origTextBytes
                       , asExtraStart       = extraAddr
                       , asExtraAddr        = extraAddr
                       , asExtraText        = mempty
                       , asAssemble         = assemble
                       , _asOrigChunks      = L.sortOn RCC.chunkAddress filteredBlocks
                       , _asAllocatedChunks = L.sortOn RCC.chunkAddress allocatedBlocks
                       , asISA              = isa
                       , asMemory           = mem
                       }
    -- Split the inputs block list into 2 lists. One for blocks that fit in the
    -- original address space and one for the newly allocated blocks.
    --
    -- We mix basic blocks and raw injected code using the 'RCC.Chunk' abstraction
    -- below.  Most of the code here doesn't really need to know anything
    -- besides the address and size of each chunk (and how to turn a chunk into
    -- a bytestring).
    allCode = concat [ map RCC.BlockChunk blocks
                     , [ RCC.RawChunk addr bytes | (_, addr, bytes) <- injectedCode ]
                     , map instructionBlockChunk injectedInsns
                     ]
    (origBlocks, allocatedBlocks) = foldr go ([],[]) allCode
    filteredBlocks = filter inText origBlocks
    inText c = absoluteAddress absStartAddr <= absoluteAddress (RCC.chunkAddress c) &&
               absoluteAddress (RCC.chunkAddress c) < absoluteAddress absEndAddr
    -- NOTE: This test (using resolveAbsoluteAddr) is depending on the fact that
    -- resolveAbsoluteAddr will fail for any address that is not in the macaw
    -- Memory (which our new code will not be)
    go c (origAcc, allocatedAcc) = case MM.resolveAbsoluteAddr mem (absoluteAddress (RCC.chunkAddress c)) of
      Nothing -> (origAcc, c:allocatedAcc)
      Just _  -> (c:origAcc, allocatedAcc)

-- | Process all the input blocks. First, look at each block that will be in the
-- original address space of the binary. Then look at each block that will be
-- newly allocated.
assembleDriver
  :: forall m arch
   . (C.MonadThrow m, MM.MemWidth (MM.ArchAddrWidth arch), HasCallStack)
  => Assembler arch m ()
assembleDriver = do
  textStart <- St.gets asTextStart
  textEnd <- St.gets asTextEnd

  let inText b = RCC.chunkAddress b >= textStart && RCC.chunkAddress b < textEnd

      loopOrig = do
        mb <- takeNextOrigBlock
        case mb of
          Just b -> do
            unless (inText b) $ do
              RP.panic RP.Assemble "assembleDriver" ["Expected this block address to be within the text section: " ++ show (RCC.chunkAddress b)]
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
            when (inText b) $ do
              RP.panic RP.Assemble "assembleDriver" ["Expected this block address to not be in the text section: " ++ show (RCC.chunkAddress b)]
            assembleAsExtra b
            loopAllocated
          Nothing -> return ()

  loopOrig

  -- double check that nobody else put something on the "to-assemble" list
  -- behind our backs
  origChunks <- L.use asOrigChunks
  unless (null origChunks) $ do
    RP.panic RP.Assemble "assembleDriver" ["Expected there to be no original chunks left"]
  allocChunks <- L.use asAllocatedChunks
  unless (null allocChunks) $ do
    RP.panic RP.Assemble "assembleDriver" ["Expected there to be no allocated chunks left"]

-- | Code in the extra section never overlaps, so we can just perform some basic
-- consistency check sand then append it.
assembleAsExtra :: (HasCallStack, C.MonadThrow m, MM.MemWidth (MM.ArchAddrWidth arch))
                => RCC.Chunk arch
                -> Assembler arch m ()
assembleAsExtra b = do
  padToBlockStartExtra b
  nextExtraAddr <- St.gets asExtraAddr
  -- traceM $ "nextExtraAddr = " ++ show nextExtraAddr
  -- traceM $ "b = " ++ show (RCC.chunkAddress b)
  unless (nextExtraAddr == RCC.chunkAddress b) $ do
    C.throwM (RCE.DiscontiguousBlocks b nextExtraAddr)
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
assembleAsText :: (HasCallStack, C.MonadThrow m, MM.MemWidth (MM.ArchAddrWidth arch))
               => RCC.Chunk arch
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
                -> ConcretizedBlock arch
                -> ConcreteAddress arch
blockEndAddress isa b =
  concretizedBlockAddress b `addressAddOffset` fromIntegral (blockSize isa b)

-- | Assemble a block with its overlapping blocks into the text section.
--
-- The block should start at the current pointer into the text section.  The
-- overlapping blocks (if any) must start immediately after the redirecting
-- jump.  The overlapping blocks must be contained and contiguous.
--
-- There may be space after the last block that will be filled with traps.
checkedOverlappingAssemble :: (HasCallStack, C.MonadThrow m, MM.MemWidth (MM.ArchAddrWidth arch))
                           => RCC.Chunk arch
                           -> [RCC.Chunk arch]
                           -> Assembler arch m ()
checkedOverlappingAssemble c overlays = do
  case c of
    RCC.RawChunk {} ->
      -- RawChunks represent new code that the caller has injected.  This code
      -- should *never* overlap any other code in the binary
      unless (null overlays) $ do
        RP.panic RP.Assemble "checkedOverlappingAssemble" ["Expected there to be no overlays overlapping this chunk: " ++ show (RCC.chunkAddress c)]
    RCC.BlockChunk b -> do
      isa <- St.gets asISA
      let blockEnd = blockEndAddress isa b
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
          RCC.RawChunk {} ->
            RP.panic RP.Assemble "checkedOverlappingAssemble" ["RawChunks cannot be overlaid blocks"]
          RCC.BlockChunk overlayBlock -> do
            let overlayEnd = blockEndAddress isa overlayBlock
            unless (concretizedBlockAddress overlayBlock > concretizedBlockAddress b) $ do
              RP.panic RP.Assemble "checkedOverlappingAssemble" [""]
            unless (overlayEnd == blockEnd) $ do
              RP.panic RP.Assemble "checkedOverlappingAssemble" [""]
            bytesB <- bytesFor b
            bytesOverlay <- bytesFor overlayBlock
            unless (bytesOverlay `B.isSuffixOf` bytesB) $ do
              RP.panic RP.Assemble "checkedOverlappingAssemble" [""]
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
                         . (HasCallStack, C.MonadThrow m, MM.MemWidth (MM.ArchAddrWidth arch))
                        => RCC.Chunk arch
                        -> Assembler arch m [RCC.Chunk arch]
lookupOverlappingBlocks b = do
  isa <- St.gets asISA
  let sz = RCC.chunkSize isa b
      blockEnd  = RCC.chunkAddress b `addressAddOffset` fromIntegral sz
  go isa blockEnd
  where
    -- Look up the next block and see if it overlaps the current block @b@.
    -- Stop looking when we find a block that clearly does not overlap.
    go :: ISA arch -> ConcreteAddress arch -> Assembler arch m [RCC.Chunk arch]
    go isa blockEnd = do
      mb' <- takeNextOrigBlock
      case mb' of
        Nothing -> return []
        Just b' -> do
          let bsize = fromIntegral (RCC.chunkSize isa b')
          case () of
            _ | RCC.chunkAddress b' >= blockEnd -> do
                  -- If the next block comes after the current block, just put
                  -- it back and return
                  asOrigChunks L.%= (b':)
                  return []
              | (RCC.chunkAddress b' `addressAddOffset` bsize) > blockEnd ->
                -- We want to assert that, if any blocks overlap, they all end
                -- at the same instruction.  If that isn't the case, it kind of
                -- indicates an inconsistent set of blocks coming from macaw or
                -- the rewriter.
                  C.throwM (RCE.OverlayBlockNotContained b b')
              | otherwise -> do
                  (b':) <$> go isa blockEnd

-- | If the given block doesn't start at the next expected address in the text
-- section, pull bytes from the original text section to pad it out until we
-- have alignment.
--
-- This covers cases where the previous block was followed by data (that was not
-- recognized as instructions by the analysis).  We have to preserve such data.
padToBlockStart :: (HasCallStack, Monad m, MM.MemWidth (MM.ArchAddrWidth arch))
                => RCC.Chunk arch
                -> Assembler arch m ()
padToBlockStart b = do
  nextAddr  <- St.gets asTextAddr
  -- startAddr <- St.gets asTextStart
  -- endAddr   <- St.gets asTextEnd
  -- Only do padding when the basic block is in the .text
  -- when (startAddr <= basicBlockAddress b && basicBlockAddress b <= endAddr) $ do
  unless (nextAddr <= RCC.chunkAddress b) $ do
    RP.panic RP.Assemble "padToBlockStart" [""]
  case nextAddr == RCC.chunkAddress b of
    True -> return ()
    False -> do
      -- traceM "generating padding"
      -- traceM $ "startAddr = " ++ show startAddr
      -- traceM $ "nextAddr  = " ++ show nextAddr
      -- traceM $ "b         = " ++ show (basicBlockAddress b)
      -- traceM $ "endAddr   = " ++ show endAddr
      origTextBytes <- St.gets asOrigTextBytes
      textStart     <- St.gets asTextStart
      let gapSize  = fromIntegral (RCC.chunkAddress b `addressDiff` nextAddr)
      let idx      = fromIntegral (nextAddr `addressDiff` textStart)
      let gapBytes = B.take gapSize (B.drop idx origTextBytes)
      -- traceM $ "gapSize = " ++ show gapSize
      -- traceM ""
      unless (B.length gapBytes == gapSize) $ do
        RP.panic RP.Assemble "padToBlockStart" [""]
      St.modify' $ \s -> s { asTextAddr    = asTextAddr s `addressAddOffset` fromIntegral gapSize
                           , asTextSection = asTextSection s <> B.byteString gapBytes
                           }

padToBlockStartExtra :: (HasCallStack, Monad m, MM.MemWidth (MM.ArchAddrWidth arch))
                     => RCC.Chunk arch
                     -> Assembler arch m ()
padToBlockStartExtra b = do
  nextExtraAddr <- St.gets asExtraAddr
  unless (nextExtraAddr <= RCC.chunkAddress b) $ do
    RP.panic RP.Assemble "padToBlockStartExtra" [""]
  case nextExtraAddr == RCC.chunkAddress b of
    True -> return ()
    False -> do
      let gapSize  = fromIntegral (RCC.chunkAddress b `addressDiff` nextExtraAddr)
      let gapBytes = B.replicate gapSize 0
      unless (B.length gapBytes == gapSize) $ do
        RP.panic RP.Assemble "padToBlockStartExtra" [""]
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
  let leftOversLen = B.length origTextBytes - idx
  let leftOvers    = B.take leftOversLen (B.drop idx origTextBytes)
  -- traceM $ "padLastBlock = " ++ show (B.length origTextBytes - idx)
  if leftOversLen > 0
     then do
       St.modify' $ \s -> s { asTextAddr    = asTextAddr s `addressAddOffset` fromIntegral leftOversLen
                            , asTextSection = asTextSection s <> B.byteString leftOvers
                            }
     else return ()

bytesFor :: forall m arch . (C.MonadThrow m) => ConcretizedBlock arch -> Assembler arch m B.ByteString
bytesFor b = do
  withConcretizedInstructions b $ \_repr insns -> do
    st <- St.get
    case mapM (asAssemble st) insns of
      Left err -> C.throwM (RCE.AssemblyError b err)
      Right strs -> return (sconcat strs)

assembleBlock :: (HasCallStack, C.MonadThrow m) => RCC.Chunk arch -> Assembler arch m B.ByteString
assembleBlock c =
  case c of
    RCC.BlockChunk b -> bytesFor b
    RCC.RawChunk _ b -> return b

-- | Helper function for taking the next block.
takeNextChunkWith :: (Monad m)
                  => L.Lens' (AssembleState arch) [RCC.Chunk arch]
                  -> Assembler arch m (Maybe (RCC.Chunk arch))
takeNextChunkWith f = do
  bs <- L.use f
  case bs of
    [] -> return Nothing
    (b:rest) -> do
      f L..= rest
      return $! Just b

-- | Grabs the next block from the orginal block set (original in the sense that
-- these blocks have addresses in the original address space).
takeNextOrigBlock :: (Monad m) => Assembler arch m (Maybe (RCC.Chunk arch))
takeNextOrigBlock = takeNextChunkWith asOrigChunks

-- | Grabs the next block from the allocated block set (allocated in the sense that
-- these blocks DO NOT have addresses in the original address space).
takeNextAllocatedChunk :: (Monad m) => Assembler arch m (Maybe (RCC.Chunk arch))
takeNextAllocatedChunk = takeNextChunkWith asAllocatedChunks

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
                , asAssemble :: forall tp . RCI.Instruction arch tp () -> Either C.SomeException B.ByteString
                -- ^ The assembler to turn instructions into bytes
                , _asOrigChunks :: [RCC.Chunk arch]
                -- ^ The blocks remaining to process. These must be ordered by
                -- address and will go into the original address space of the
                -- binary.
                , _asAllocatedChunks :: [RCC.Chunk arch]
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
                }

asOrigChunks :: L.Lens' (AssembleState arch) [RCC.Chunk arch]
asOrigChunks = L.lens _asOrigChunks (\as bs -> as { _asOrigChunks = bs })

asAllocatedChunks :: L.Lens' (AssembleState arch) [RCC.Chunk arch]
asAllocatedChunks = L.lens _asAllocatedChunks (\as bs -> as { _asAllocatedChunks = bs })

fromBuilder :: B.Builder -> B.ByteString
fromBuilder = LBS.toStrict . B.toLazyByteString
