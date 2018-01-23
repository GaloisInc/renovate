{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
-- | The interface for 'BasicBlock' recovery
module Renovate.Recovery (
  recoverBlocks,
  BlockInfo(..),
  ArchBits,
  Diagnostic(..)
  ) where

import qualified GHC.Err.Located as L
import qualified Control.Exception as E
import qualified Control.Lens as L
import qualified Control.Monad.Catch as C
import           Control.Monad.ST ( stToIO, ST, RealWorld )
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NEL
import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Maybe ( catMaybes, fromMaybe, mapMaybe )
import qualified Data.Set as S

import qualified Data.Macaw.Architecture.Info as MC
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MC
import qualified Data.Macaw.Memory as MC
import qualified Data.Macaw.Types as MC
import qualified Data.Parameterized.Some as PU

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.ISA
import           Renovate.Recovery.Monad
import           Renovate.Redirect.Monad ( SymbolMap )

type ArchBits arch w = (w ~ MC.RegAddrWidth (MC.ArchReg arch),
                        MC.ArchConstraints arch,
                        MC.RegisterInfo (MC.ArchReg arch),
                        MC.HasRepr (MC.ArchReg arch) MC.TypeRepr,
                        MC.MemWidth w,
                        Show (MC.ArchReg arch (MC.BVType (MC.ArchAddrWidth arch))))

-- | Information on recovered basic blocks
data BlockInfo i w arch = BlockInfo
  { biBlocks           :: [ConcreteBlock i w]
  , biFunctionEntries  :: [ConcreteAddress w]
  , biFunctionBlocks   :: M.Map (ConcreteAddress w) [ConcreteBlock i w]
  , biDiscoveryFunInfo :: M.Map (ConcreteAddress w) (PU.Some (MC.DiscoveryFunInfo arch))
  }

analyzeDiscoveredFunctions :: (MC.MemWidth (MC.ArchAddrWidth arch))
                           => ISA i a (MC.ArchAddrWidth arch)
                           -> (forall m . (C.MonadThrow m)  => B.ByteString -> m (Int, i ()))
                           -> MC.Memory (MC.ArchAddrWidth arch)
                           -> Maybe (MC.ArchSegmentOff arch -> ST RealWorld ())
                           -> Maybe (Int, MC.ArchSegmentOff arch -> Either C.SomeException (BlockInfo i (MC.ArchAddrWidth arch) arch) -> IO ())
                           -> MC.DiscoveryState arch
                           -> Int
                           -- ^ Iteration count
                           -> IO (MC.DiscoveryState arch)
analyzeDiscoveredFunctions isa dis1 mem blockCallback funcCallback info !iterations =
  case M.lookupMin (info L.^. MC.unexploredFunctions) of
    Nothing -> return info
    Just (addr, rsn) -> do
      (info', _) <- stToIO (MC.analyzeFunction (fromMaybe (const (return ())) blockCallback) addr rsn info)
      case funcCallback of
        Just (freq, fcb)
          | iterations `mod` freq == 0 -> fcb addr (blockInfo isa dis1 mem info')
        _ -> return ()
      analyzeDiscoveredFunctions isa dis1 mem blockCallback funcCallback info' (iterations + 1)

cfgFromAddrsWith :: (MC.MemWidth (MC.ArchAddrWidth arch))
                 => ISA i a (MC.ArchAddrWidth arch)
                 -> (forall m . (C.MonadThrow m)  => B.ByteString -> m (Int, i ()))
                 -> Maybe (MC.ArchSegmentOff arch -> ST RealWorld ())
                 -> Maybe (Int, MC.ArchSegmentOff arch -> Either C.SomeException (BlockInfo i (MC.ArchAddrWidth arch) arch) -> IO ())
                 -> MC.ArchitectureInfo arch
                 -> MC.Memory (MC.ArchAddrWidth arch)
                 -> MC.AddrSymMap (MC.ArchAddrWidth arch)
                 -> [MC.ArchSegmentOff arch]
                 -> [(MC.ArchSegmentOff arch, MC.ArchSegmentOff arch)]
                 -> IO (MC.DiscoveryState arch)
cfgFromAddrsWith isa dis1 blockCallback funcCallback archInfo mem symbols initAddrs memWords = do
  let s1 = MC.markAddrsAsFunction MC.InitAddr initAddrs s0
  s2 <- analyzeDiscoveredFunctions isa dis1 mem blockCallback funcCallback s1 0
  let s3 = MC.exploreMemPointers memWords s2
  analyzeDiscoveredFunctions isa dis1 mem blockCallback funcCallback s3 0
  where
    s0 = MC.emptyDiscoveryState mem symbols archInfo

blockInfo :: (MC.MemWidth (MC.RegAddrWidth (MC.ArchReg arch)), C.MonadThrow m)
          => ISA i a (MC.RegAddrWidth (MC.ArchReg arch))
          -> (B.ByteString -> Maybe (Int, i ()))
          -> MC.Memory (MC.RegAddrWidth (MC.ArchReg arch))
          -> MC.DiscoveryState arch
          -> m (BlockInfo i (MC.RegAddrWidth (MC.ArchReg arch)) arch)
blockInfo isa dis1 mem di = do
  let blockBuilder = buildBlock isa dis1 mem (S.fromList (mapMaybe (concreteFromSegmentOff mem) (F.toList absoluteBlockStarts)))
  blocks <- catMaybes <$> mapM blockBuilder (F.toList absoluteBlockStarts)
  let addBlock m b = M.insert (basicBlockAddress b) b m
  let blockIndex = F.foldl' addBlock M.empty blocks
  let funcBlocks = M.fromList [ (funcAddr, mapMaybe (\a -> M.lookup a blockIndex) blockAddrs)
                              | PU.Some dfi <- M.elems (di L.^. MC.funInfo)
                              , Just funcAddr <- [concreteFromSegmentOff mem (MC.discoveredFunAddr dfi)]
                              , let blockAddrs = mapMaybe (concreteFromSegmentOff mem) (M.keys (dfi L.^. MC.parsedBlocks))
                              ]
  return BlockInfo { biBlocks = blocks
                   , biFunctionEntries = mapMaybe (concreteFromSegmentOff mem) funcEntries
                   , biFunctionBlocks = funcBlocks
                   , biDiscoveryFunInfo = infos
                   }
  where
    absoluteBlockStarts = S.fromList [ entry
                                     | PU.Some dfi <- M.elems (di L.^. MC.funInfo)
                                     , entry <- M.keys (dfi L.^. MC.parsedBlocks)
                                     ]
    funcEntries = [ MC.discoveredFunAddr dfi
                  | PU.Some dfi <- MC.exploredFunctions di
                  ]
    infos = M.fromList [ (concAddr, val)
                       | (segOff, val) <- M.toList (di L.^. MC.funInfo)
                       , Just concAddr <- return (concreteFromSegmentOff mem segOff)
                       ]

recoverBlocks :: (ArchBits arch w)
              => Maybe (MC.ArchSegmentOff arch -> ST RealWorld ())
              -> Maybe (Int, MC.ArchSegmentOff arch -> Either C.SomeException (BlockInfo i w arch) -> IO ())
              -> ISA i a w
              -> (forall m . (C.MonadThrow m)  => B.ByteString -> m (Int, i ()))
              -- ^ A function to try to disassemble a single
              -- instruction from a bytestring, returning the number
              -- of bytes consumed and the instruction if successful
              -> MC.ArchitectureInfo arch
              -> MC.Memory w
              -> SymbolMap w
              -> NEL.NonEmpty (MC.MemSegmentOff w)
              -- ^ A list of entry points in the memory space
              -> IO (Either E.SomeException (BlockInfo i w arch))
recoverBlocks blockCallback funcCallback isa dis1 archInfo mem symmap entries = do
  sam <- toMacawSymbolMap mem symmap
  di <- cfgFromAddrsWith isa dis1 blockCallback funcCallback archInfo mem sam (F.toList entries) []
  return (blockInfo isa dis1 mem di)

toMacawSymbolMap :: (MC.MemWidth w) => MC.Memory w -> SymbolMap w -> IO (MC.AddrSymMap w)
toMacawSymbolMap mem sm = return (M.mapKeys toSegOff sm)
  where
    toSegOff concAddr =
      case concreteAsSegmentOff mem concAddr of
        Nothing -> error ("Invalid concrete address: " ++ show concAddr)
        Just so -> so


-- | Build our representation of a basic block from a provided block
-- start address
--
-- The block starts are obtained from Macaw.  We disassemble from the
-- start address until we hit a terminator instruction or the start of
-- another basic block.
buildBlock :: (L.HasCallStack, MC.MemWidth w, C.MonadThrow m)
           => ISA i a w
           -> (B.ByteString -> Maybe (Int, i ()))
           -- ^ The function to pull a single instruction off of the
           -- byte stream; it returns the number of bytes consumed and
           -- the new instruction.
           -> MC.Memory w
           -> S.Set (ConcreteAddress w)
           -- ^ The set of all basic block entry points
           -> MC.MemSegmentOff w
           -- ^ The address to start disassembling this block from
           -> m (Maybe (ConcreteBlock i w))
buildBlock isa dis1 mem absStarts segAddr
  | Just concAddr <- concreteFromSegmentOff mem segAddr = do
      case MC.addrContentsAfter mem (MC.relativeSegmentAddr segAddr) of
        Left err -> C.throwM (MemoryError err)
        Right [MC.ByteRegion bs] -> Just <$> go concAddr concAddr bs []
        _ -> C.throwM (NoByteRegionAtAddress (MC.relativeSegmentAddr segAddr))
  | otherwise = return Nothing
  where
    addOff       = addressAddOffset  mem
    go blockAbsAddr insnAddr bs insns = do
      case dis1 bs of
        -- Actually, we should probably never hit this case.  We
        -- should have hit a terminator instruction or end of block
        -- before running out of bytes...
        Nothing -> return BasicBlock { basicBlockAddress = blockAbsAddr
                                     , basicBlockInstructions = reverse insns
                                     }
        Just (bytesRead, i)
          -- The next instruction we would decode starts another
          -- block, OR the instruction we just decoded is a
          -- terminator, so end the block and stop decoding
          | S.member (insnAddr `addOff` fromIntegral bytesRead) absStarts ||
            isaJumpType isa i mem insnAddr /= NoJump -> do
            return BasicBlock { basicBlockAddress      = blockAbsAddr
                              , basicBlockInstructions = reverse (i : insns)
                              }

          -- Otherwise, we just keep decoding
          | otherwise -> go blockAbsAddr (insnAddr `addOff` fromIntegral bytesRead) (B.drop bytesRead bs) (i : insns)


{- Note [Unaligned Instructions]

We still need to raise an error if we see a jump into an unaligned
instruction.  It is technically perfectly possible, but we don't want
to support or encourage it.

-}
