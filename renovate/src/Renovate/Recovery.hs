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

import qualified Control.Exception as E
import qualified Control.Lens as L
import qualified Control.Monad.Catch as C
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NEL
import qualified Data.Foldable as F
import qualified Data.Map as M
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

type ArchBits arch w = (w ~ MC.RegAddrWidth (MC.ArchReg arch),
                  MC.PrettyF (MC.ArchStmt arch),
                  MC.ArchConstraints arch,
                  MC.RegisterInfo (MC.ArchReg arch),
                  MC.HasRepr (MC.ArchReg arch) MC.TypeRepr,
                  MC.MemWidth w,
                  -- MC.PrettyCFGConstraints arch,
                  Show (MC.ArchReg arch (MC.BVType (MC.ArchAddrWidth arch))))

-- | Information on recovered basic blocks
data BlockInfo i w arch = BlockInfo
  { biBlocks           :: [ConcreteBlock i w]
  , biFunctionEntries  :: [RelAddress w]
  , biFunctionBlocks   :: M.Map (RelAddress w) [ConcreteBlock i w]
  , biDiscoveryFunInfo :: M.Map (RelAddress w) (PU.Some (MC.DiscoveryFunInfo arch))
  }

recoverBlocks :: (ArchBits arch w)
              => ISA i a w
              -> (forall m . (C.MonadThrow m)  => B.ByteString -> m (Int, i ()))
              -- ^ A function to try to disassemble a single
              -- instruction from a bytestring, returning the number
              -- of bytes consumed and the instruction if successful
              -> MC.ArchitectureInfo arch
              -> MC.Memory w
              -> NEL.NonEmpty (MC.MemSegmentOff w)
              -- ^ A list of entry points in the memory space
              -> (Either E.SomeException (BlockInfo i w arch), [Diagnostic])
recoverBlocks isa dis1 archInfo mem entries = runRecovery isa dis1 mem $ do
   let di = MC.cfgFromAddrs archInfo mem MC.emptySymbolAddrMap (F.toList entries) []
       absoluteBlockStarts = S.fromList [ entry
                                        | PU.Some dfi <- M.elems (di  L.^. MC.funInfo)
                                        , entry       <- {- trace (show (pretty dfi)) $ -} M.keys  (dfi L.^. MC.parsedBlocks)
                                        ]
       funcEntries = [ MC.discoveredFunAddr dfi
                     | PU.Some dfi <- MC.exploredFunctions di
                     ]
       infos = M.mapKeys (relFromSegmentOff mem) (di L.^. MC.funInfo)
   -- traceM ("unexplored functions: " ++ show (di L.^. MC.unexploredFunctions))
   -- traceM ("explored functions: " ++ show [pretty i | PU.Some i <- MC.exploredFunctions di])
   -- traceM ("Discovered block starts: " ++ show absoluteBlockStarts)
   blocks <- mapM (buildBlock isa dis1 mem (S.map (relFromSegmentOff mem) absoluteBlockStarts))
                  (F.toList absoluteBlockStarts)
   let funcBlocks        = foldr insertBlocks M.empty blocks
       insertBlocks cb m = M.adjust (cb:) (basicBlockAddress cb) m
   return BlockInfo { biBlocks           = blocks
                    , biFunctionEntries  = map (relFromSegmentOff mem) funcEntries
                    , biFunctionBlocks   = funcBlocks
                    , biDiscoveryFunInfo = infos
                    }

-- | Build our representation of a basic block from a provided block
-- start address
--
-- The block starts are obtained from Macaw.  We disassemble from the
-- start address until we hit a terminator instruction or the start of
-- another basic block.
buildBlock :: (MC.MemWidth w)
           => ISA i a w
           -> (B.ByteString -> Maybe (Int, i ()))
           -- ^ The function to pull a single instruction off of the
           -- byte stream; it returns the number of bytes consumed and
           -- the new instruction.
           -> MC.Memory w
           -> S.Set (RelAddress w)
           -- ^ The set of all basic block entry points
           -> MC.MemSegmentOff w
           -- ^ The address to start disassembling this block from
           -> Recovery i a w (ConcreteBlock i w)
buildBlock isa dis1 mem absStarts segAddr = do
  case MC.addrContentsAfter mem (MC.relativeSegmentAddr segAddr) of
    Left err -> C.throwM (MemoryError err)
    Right [MC.ByteRegion bs] -> go blockAbsAddr bs []
    _ -> C.throwM (NoByteRegionAtAddress (MC.relativeSegmentAddr segAddr))
  where
    blockAbsAddr = relFromSegmentOff mem segAddr
    go insnAddr bs insns = do
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
          | S.member (insnAddr `addressAddOffset` fromIntegral bytesRead) absStarts ||
            isaJumpType isa i mem insnAddr /= NoJump -> do
            return BasicBlock { basicBlockAddress = blockAbsAddr
                              , basicBlockInstructions =  reverse (i : insns)
                              }

          -- Otherwise, we just keep decoding
          | otherwise -> go (insnAddr `addressAddOffset` fromIntegral bytesRead) (B.drop bytesRead bs) (i : insns)


{- Note [Unaligned Instructions]

We still need to raise an error if we see a jump into an unaligned
instruction.  It is technically perfectly possible, but we don't want
to support or encourage it.

-}
