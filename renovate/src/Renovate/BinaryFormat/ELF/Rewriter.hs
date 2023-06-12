{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Renovate.BinaryFormat.ELF.Rewriter (
  ElfRewriter,
  runElfRewriter,
  RewriterState(..),
  RewriterInfo(..),
  logDiagnostic,
  RewriterEnv,
  reSegmentMaximumSize,
  reSegmentVirtualAddress,
  reLogAction,
  reTextSectionName
  ) where

import qualified Control.Monad.Catch as CMC
import qualified Control.Monad.Fail as MF
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Strict as S
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import qualified Lumberjack as LJ

import           Prelude

import qualified Data.ElfEdit as E
import qualified Data.Macaw.CFG as MM

import qualified Renovate.BinaryFormat.ELF.Common as RBEC
import qualified Renovate.Config as RC
import qualified Renovate.Core.Diagnostic as RCD
import qualified Renovate.Core.Exception as RCE
import qualified Renovate.Core.Instruction as RCI
import qualified Renovate.Core.Layout as RT
import qualified Renovate.ISA as ISA
import qualified Renovate.Recovery as RRe
import qualified Renovate.Redirect as RR

-- | Read-only environment for ELF rewriting
data RewriterEnv lm arch =
  RewriterEnv { reSegmentVirtualAddress :: E.ElfWordType (MM.ArchAddrWidth arch)
              -- ^ Address of the new text section
              , reSegmentMaximumSize :: E.ElfWordType (MM.ArchAddrWidth arch)
              -- ^ Maximum size of the new text section
              , reLogAction :: LJ.LogAction IO (RCD.Diagnostic lm)
              -- ^ The logger used by the Rewriter monad
              , reTextSectionName :: String
              -- ^ The name of the text section including the leading
              -- dot, e.g. ".text"
              }

makeRewriterEnv
  :: ( CMC.MonadThrow m
     , E.ElfWidthConstraints (MM.ArchAddrWidth arch)
     )
  => LJ.LogAction IO (RCD.Diagnostic lm)
  -> RC.RenovateConfig arch binFmt callbacks b
  -> E.Elf (MM.ArchAddrWidth arch)
  -> m (RewriterEnv lm arch)
makeRewriterEnv logAction cfg e = do
  -- Compute the address to start laying out new code.
  --
  -- The new code's virtual address should satisfy a few constraints:
  --
  -- 1. Not overlaying any existing loadable segments.
  -- 2. Within jumping range of the old text section to ease redirection of blocks.
  -- 3. Enough empty space to hold the new code.
  --
  -- It's real tough to guarantee (3), since we don't know how much code there
  -- will be yet. So we just pick the biggest chunk of address space that
  -- satisfies (1) and (2).
  let textSecName = RC.rcTextSectionName cfg
  textSection <- RBEC.findTextSection textSecName e
  let (lo, hi) = withinJumpRange cfg textSection
  let layoutChoiceFunction = case RC.rcExtratextOffset cfg of
        0 -> selectLayoutAddr lo hi
        n | n < 0 -> computeSizeOfLayoutAddr (E.elfSectionAddr textSection + fromIntegral n)
          | otherwise -> computeSizeOfLayoutAddr (E.elfSectionAddr textSection + E.elfSectionSize textSection + fromIntegral n)
  (newTextAddr, newTextSize) <- layoutChoiceFunction (fromIntegral RBEC.newTextAlign) e
  pure $ RewriterEnv { reSegmentVirtualAddress = fromIntegral newTextAddr
                     , reSegmentMaximumSize = fromIntegral newTextSize
                     , reLogAction = logAction
                     , reTextSectionName = textSecName
                     }


-- | Given an existing section, find the range of addresses where we could lay
-- out code while still being able to jump to anywhere in the existing section.
withinJumpRange
  :: ( w ~ E.ElfWordType (MM.ArchAddrWidth arch)
     , Num w
     , Ord w
     )
  => RC.RenovateConfig arch binFmt callbacks b
  -> E.ElfSection w
  -> (w, w)
withinJumpRange cfg text =
  -- max 1: we don't want to lay out code at address 0...
  ( max 1 (end - min end range)
  , start + range
  )
  where
  start = E.elfSectionAddr text
  end = start + E.elfSectionSize text - 1
  isa = RC.rcISA cfg
  -- FIXME: This is wrong for multi-arch ISAs (i.e., ARM).  We only have 16 bits
  -- of jump offset from Thumb code, which is potentially a huge problem.
  --
  -- It may not be a significant problem if we use the trick of embedding the
  -- jump address in the code section nearby so that we could jump arbitrarily
  -- far.
  range = case ISA.isaDefaultInstructionArchRepr isa of
    RCI.SomeInstructionArchRepr repr -> fromIntegral (ISA.isaMaxRelativeJumpSize isa repr)


-- | Choose a virtual address for extratext (and report how many bytes are
-- available at that address).
selectLayoutAddr ::
  ( CMC.MonadThrow m
  , E.ElfWidthConstraints w
  ) =>
  E.ElfWordType w ->
  E.ElfWordType w ->
  E.ElfWordType w ->
  E.Elf w ->
  m (E.ElfWordType w, E.ElfWordType w)
selectLayoutAddr lo hi alignment e = do
  allocated <- RBEC.allocatedVAddrsM e
  case availableAddrs lo hi alignment allocated of
    [] -> CMC.throwM RCE.NoUnallocatedVirtualAddressSpace
    available -> pure $ L.maximumBy (O.comparing snd) available


-- | Find a region suitable for the requested layout address
--
-- Note that the caller cannot easily tell what address to request that actually respects alignment
-- constraints, so this code aligns the address before looking for an allocation site.
computeSizeOfLayoutAddr
  :: forall m w .
  ( CMC.MonadThrow m
  , E.ElfWidthConstraints w
  ) =>
  E.ElfWordType w ->
  E.ElfWordType w ->
  E.Elf w ->
  m (E.ElfWordType w, E.ElfWordType w)
computeSizeOfLayoutAddr addr alignment e = do
  let alignedAddr = RBEC.alignValue addr alignment
  allocated <- RBEC.allocatedVAddrsM e
  case availableAddrs alignedAddr maxBound alignment allocated of
    (result@(addr',_)):_
      | alignedAddr == addr' -> return result
      | addr' < alignedAddr + alignment ->
        CMC.throwM (RCE.LayoutNotByteAligned @w alignedAddr alignment)
    _ -> CMC.throwM (RCE.LayoutAddressOverlapsExistingSegments @w alignedAddr)


-- | Generate a list of @(addr, size)@ pairs where each pair is within @(lo,
-- hi)@ (i.e., in range of a jump from the text section) and correctly aligned
-- w.r.t. the passed-in alignment.
availableAddrs :: (Ord w, Integral w) => w -> w -> w -> M.Map w w -> [(w, w)]
availableAddrs lo hi alignment allocated = go lo (M.toAscList allocated)
  where
    -- This function scans the list of already-allocated address ranges and puts
    -- together a list of pairs representing unallocated ranges.  The @addr@
    -- argument is the start of the next available range.
    go addr allocd =
      case allocd of
        [] ->
          -- In this case, we are out of allocated pairs.  We'll make one last
          -- address range starting here and spanning to the end of the
          -- reachable range
          buildAlignedRange addr hi
        (_, 0) : rest ->
          -- In this case, we hit a zero-sized range.  Just skip it
          go addr rest
        (allocatedStart, allocatedSize) : rest
          -- If we reach a region beyond the high watermark, just throw the rest away
          | allocatedStart >= hi -> []
          | addr < allocatedStart ->
            buildAlignedRange addr allocatedStart ++ go (allocatedStart + allocatedSize) rest
          -- Should never happen, but just in case...
          | otherwise -> go addr rest

    buildAlignedRange base0 end0
      | base >= hi || end <= lo || base >= end = []
      | otherwise = [(base, end - base)]
      where
        base = (base0-1) + alignment - ((base0-1) `mod` alignment)
        end = min end0 hi

data RewriterState arch =
  RewriterState { _rsELF :: E.Elf (MM.ArchAddrWidth arch)
                , _rsInitialELFHeader :: E.ElfHeaderInfo (MM.ArchAddrWidth arch)
                }

-- | Statistics gathered and diagnostics generated during the
-- rewriting phase.
data RewriterInfo arch =
  RewriterInfo { riRedirectionResult :: RR.RedirectionResult arch
               -- ^ Low-level results from the block redirection process
               , riRewritePairs :: [RT.RewritePair arch]
               -- ^ The mapping that describes unchanged blocks or the rewritten
               -- version of a block
               , riSegmentVirtualAddress :: E.ElfWordType (MM.ArchAddrWidth arch)
               -- ^ The address of the new text segment
               , riBlockInfo :: RRe.BlockInfo arch
               -- ^ The results of code discovery
               }

newtype ElfRewriter lm arch a =
  ElfRewriter { unElfRewrite :: RWS.RWST  (RewriterEnv lm arch) () (RewriterState arch) IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , IO.MonadIO
    , MF.MonadFail
    , CMC.MonadThrow
    , S.MonadState (RewriterState arch)
    , R.MonadReader (RewriterEnv lm arch)
    )

runElfRewriter
  :: E.ElfWidthConstraints (MM.ArchAddrWidth arch)
  => LJ.LogAction IO (RCD.Diagnostic lm)
  -> RC.RenovateConfig arch binFmt callbacks b
  -> E.ElfHeaderInfo (MM.ArchAddrWidth arch)
  -> E.Elf (MM.ArchAddrWidth arch)
  -> ElfRewriter lm arch a
  -> IO (a, RewriterState arch, RewriterEnv lm arch)
runElfRewriter logAction config ehi e a = do
  env <- makeRewriterEnv logAction config e
  (result, info, ()) <- RWS.runRWST (unElfRewrite a) env (initialRewriterState ehi e)
  return (result, info, env)

initialRewriterState
  :: E.ElfHeaderInfo (MM.ArchAddrWidth arch)
  -> E.Elf (MM.ArchAddrWidth arch)
  -> RewriterState arch
initialRewriterState ehi e =
  RewriterState { _rsELF                      = e
                , _rsInitialELFHeader         = ehi
                }

logDiagnostic :: RCD.Diagnostic lm -> ElfRewriter lm arch ()
logDiagnostic d = do
  la <- R.asks reLogAction
  IO.liftIO $ LJ.writeLog la d

