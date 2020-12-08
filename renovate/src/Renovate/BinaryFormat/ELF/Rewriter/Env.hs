{-
Module           : Renovate.BinaryFormat.ELF.Rewriter.Env
Description      : Construct the read-only environment for ELF rewriting
Copyright        : (c) Galois, Inc 2020
License          : BSD3
Maintainer       : Tristan Ravitch <tristan@galois.com>
Stability        : provisional

This currently consists only of locating a good start address for the additional
text section.
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Renovate.BinaryFormat.ELF.Rewriter.Env
  ( RewriterEnv
  , makeRewriterEnv
  , reSegmentMaximumSize
  , reSegmentVirtualAddress
  , reLogAction
  ) where

import           GHC.Generics (Generic)

import qualified Control.Monad.Catch as C
import qualified Control.Monad.Fail as Fail
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Ord as O
import qualified Lumberjack as LJ

import qualified Data.ElfEdit as E
import qualified Data.Macaw.CFG as MM

import qualified Renovate.BasicBlock as RB
import           Renovate.Config
import qualified Renovate.ISA as RI

import qualified Renovate.Diagnostic as RD
import           Renovate.BinaryFormat.ELF.Common

-- | Read-only environment for ELF rewriting
data RewriterEnv arch =
  RewriterEnv { reSegmentVirtualAddress :: E.ElfWordType (MM.ArchAddrWidth arch)
              -- ^ Address of the new text section
              , reSegmentMaximumSize :: E.ElfWordType (MM.ArchAddrWidth arch)
              -- ^ Maximum size of the new text section
              , reLogAction :: LJ.LogAction IO RD.Diagnostic
              }
  deriving Generic

makeRewriterEnv ::
  ( Fail.MonadFail m
  , C.MonadThrow m
  , E.ElfWidthConstraints (MM.ArchAddrWidth arch)
  ) =>
  LJ.LogAction IO RD.Diagnostic ->
  RenovateConfig arch binFmt callbacks b ->
  E.Elf (MM.ArchAddrWidth arch) ->
  m (RewriterEnv arch)
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
  textSection <- findTextSection e
  let (lo, hi) = withinJumpRange cfg textSection
  let layoutChoiceFunction = case rcExtratextOffset cfg of
        0 -> selectLayoutAddr lo hi
        n | n < 0 -> computeSizeOfLayoutAddr (E.elfSectionAddr textSection + fromIntegral n)
          | otherwise -> computeSizeOfLayoutAddr (E.elfSectionAddr textSection + E.elfSectionSize textSection + fromIntegral n)
  (newTextAddr, newTextSize) <- layoutChoiceFunction (fromIntegral newTextAlign) e
  pure $ RewriterEnv { reSegmentVirtualAddress = fromIntegral newTextAddr
                     , reSegmentMaximumSize = fromIntegral newTextSize
                     , reLogAction = logAction
                     }


-- | Given an existing section, find the range of addresses where we could lay
-- out code while still being able to jump to anywhere in the existing section.
withinJumpRange ::
  (w ~ E.ElfWordType (MM.ArchAddrWidth arch), Num w, Ord w) =>
  RenovateConfig arch binFmt callbacks b ->
  E.ElfSection w ->
  (w, w)
withinJumpRange cfg text =
  -- max 1: we don't want to lay out code at address 0...
  ( max 1 (end - min end range)
  , start + range
  )
  where
  start = E.elfSectionAddr text
  end = start + E.elfSectionSize text - 1
  isa = rcISA cfg
  -- FIXME: This is wrong for multi-arch ISAs (i.e., ARM).  We only have 16 bits
  -- of jump offset from Thumb code, which is potentially a huge problem.
  range = case RI.isaDefaultInstructionArchRepr isa of
    RB.SomeInstructionArchRepr repr -> fromIntegral (RI.isaMaxRelativeJumpSize isa repr)


-- | Choose a virtual address for extratext (and report how many bytes are
-- available at that address).
selectLayoutAddr ::
  ( Fail.MonadFail m
  , E.ElfWidthConstraints w
  ) =>
  E.ElfWordType w ->
  E.ElfWordType w ->
  E.ElfWordType w ->
  E.Elf w ->
  m (E.ElfWordType w, E.ElfWordType w)
selectLayoutAddr lo hi alignment e = do
  allocated <- allocatedVAddrsM e
  case availableAddrs lo hi alignment allocated of
    [] -> fail "No unallocated virtual address space within jumping range of the text section is available for use as a new extratext section."
    available -> pure $ L.maximumBy (O.comparing snd) available


-- | Find a region suitable for the requested layout address
--
-- Note that the caller cannot easily tell what address to request that actually respects alignment
-- constraints, so this code aligns the address before looking for an allocation site.
computeSizeOfLayoutAddr ::
  ( Fail.MonadFail m
  , E.ElfWidthConstraints w
  ) =>
  E.ElfWordType w ->
  E.ElfWordType w ->
  E.Elf w ->
  m (E.ElfWordType w, E.ElfWordType w)
computeSizeOfLayoutAddr addr alignment e = do
  let alignedAddr = alignValue addr alignment
  allocated <- allocatedVAddrsM e
  case availableAddrs alignedAddr maxBound alignment allocated of
    (result@(addr',_)):_
      | alignedAddr == addr' -> return result
      | addr' < alignedAddr + alignment -> fail $ "Requested layout address " ++ show alignedAddr ++ " not aligned to " ++ show alignment ++ "-byte boundary."
    _ -> fail $ "Requested layout address " ++ show alignedAddr ++ " overlaps existing segments."


-- | Generate a list of @(addr, size)@ pairs where each pair is within @(lo,
-- hi)@ (i.e., in range of a jump from the text section) and correctly aligned
-- w.r.t. the passed-in alignment.
availableAddrs :: (Ord w, Integral w) => w -> w -> w -> Map.Map w w -> [(w, w)]
availableAddrs lo hi alignment allocated = go lo (Map.toAscList allocated)
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
