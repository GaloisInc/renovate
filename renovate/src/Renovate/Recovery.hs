{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- | The interface for 'BasicBlock' recovery
module Renovate.Recovery (
  Recovery(..),
  recoverBlocks,
  BlockInfo(..),
  SymbolicCFG,
  SymbolicRegCFG,
  getSymbolicCFG,
  getSymbolicRegCFG,
  isIncompleteBlockAddress,
  ArchBits,
  ArchInfo(..),
  ArchVals(..),
  Diagnostic(..)
  ) where

import qualified GHC.Err.Located as L
import qualified Control.Lens as L
import qualified Control.Monad.Catch as C
import           Control.Monad ( guard )
import           Control.Monad.ST ( stToIO, ST, RealWorld )
import qualified Data.ByteString as B
import qualified Data.List.NonEmpty as NEL
import qualified Data.Foldable as F
import qualified Data.IORef as IO
import qualified Data.Map as M
import           Data.Maybe ( catMaybes, fromMaybe, mapMaybe )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Traversable as T

import qualified Data.Macaw.Architecture.Info as MC
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MC
import qualified Data.Macaw.Types as MC
import qualified Data.Macaw.Symbolic as MS
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Some as PU
import qualified Lang.Crucible.Backend as C
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.CFG.Extension as C
import qualified Lang.Crucible.CFG.Reg as CR
import qualified Lang.Crucible.FunctionHandle as C
import qualified What4.FunctionName as C
import qualified What4.ProgramLoc as C

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.Diagnostic
import           Renovate.ISA
import           Renovate.Redirect.Monad ( SymbolMap )
import           Renovate.Recovery.Overlap

type ArchBits arch = (  MC.ArchConstraints arch,
                        MC.RegisterInfo (MC.ArchReg arch),
                        MC.HasRepr (MC.ArchReg arch) MC.TypeRepr,
                        MC.MemWidth (MC.ArchAddrWidth arch),
                        ArchInfo arch,
                        Show (MC.ArchReg arch (MC.BVType (MC.ArchAddrWidth arch))),
                        C.IsSyntaxExtension (MS.MacawExt arch))

data Cached a = Cached (IO.IORef (Maybe a)) (IO a)
type SCFG f arch = f (MS.MacawExt arch) (Ctx.EmptyCtx Ctx.::> MS.ArchRegStruct arch) (MS.ArchRegStruct arch)
newtype SymbolicCFG arch = SymbolicCFG (Cached (SCFG C.SomeCFG arch))
newtype SymbolicRegCFG arch = SymbolicRegCFG (Cached (SCFG CR.SomeCFG arch))

-- | Construct or return the cached symbolic CFG
--
-- We have this representation to let us lazily construct CFGs, as we won't
-- usually need all of them
getSymbolicCFG :: SymbolicCFG arch -> IO (C.SomeCFG (MS.MacawExt arch) (Ctx.EmptyCtx Ctx.::> MS.ArchRegStruct arch) (MS.ArchRegStruct arch))
getSymbolicCFG (SymbolicCFG cached) = getCached cached

-- | Construct or return the cached symbolic registerized CFG
getSymbolicRegCFG :: SymbolicRegCFG arch -> IO (CR.SomeCFG (MS.MacawExt arch) (Ctx.EmptyCtx Ctx.::> MS.ArchRegStruct arch) (MS.ArchRegStruct arch))
getSymbolicRegCFG (SymbolicRegCFG cached) = getCached cached

getCached :: Cached a -> IO a
getCached (Cached r gen) = do
  v0 <- IO.readIORef r
  case v0 of
    Just c -> return c
    Nothing -> do
      c <- gen
      IO.writeIORef r (Just c)
      return c

-- | Information on recovered basic blocks
data BlockInfo arch = BlockInfo
  { biBlocks           :: [ConcreteBlock arch]
  , biFunctionEntries  :: [ConcreteAddress arch]
  , biFunctionBlocks   :: M.Map (ConcreteAddress arch) [ConcreteBlock arch]
  , biDiscoveryFunInfo :: M.Map (ConcreteAddress arch) (PU.Some (MC.DiscoveryFunInfo arch))
  , biIncomplete       :: S.Set (ConcreteAddress arch)
  -- ^ The set of blocks that reside in incomplete functions (i.e., functions
  -- for which we cannot find all of the code)
  , biCFG              :: M.Map (ConcreteAddress arch) (SymbolicCFG arch)
  -- ^ The Crucible CFG for each function (if possible to construct), see
  -- Note [CrucibleCFG]
  , biRegCFG           :: M.Map (ConcreteAddress arch) (SymbolicRegCFG arch)
  , biOverlap          :: BlockRegions arch
  -- ^ A structure that lets us determine which blocks in the program overlap
  -- other blocks in the program (so that we can avoid every trying to rewrite them)
  }

isIncompleteBlockAddress :: BlockInfo arch -> ConcreteAddress arch -> Bool
isIncompleteBlockAddress bi a = S.member a (biIncomplete bi)

analyzeDiscoveredFunctions :: (ArchBits arch)
                           => Recovery arch
                           -> MC.Memory (MC.ArchAddrWidth arch)
                           -> (ConcreteAddress arch, ConcreteAddress arch)
                           -> MC.DiscoveryState arch
                           -> Int
                           -- ^ Iteration count
                           -> IO (MC.DiscoveryState arch)
analyzeDiscoveredFunctions recovery mem textAddrRange info !iterations =
  case M.lookupMin (info L.^. MC.unexploredFunctions) of
    Nothing -> return info
    Just (addr, rsn) -> do
      let bcb = fromMaybe (const (return ())) (recoveryBlockCallback recovery)
      (info', _) <- stToIO (MC.analyzeFunction bcb addr rsn info)
      case recoveryFuncCallback recovery of
        Just (freq, fcb)
          | iterations `mod` freq == 0 -> do
              bi <- blockInfo recovery mem textAddrRange info'
              fcb addr bi
        _ -> return ()
      analyzeDiscoveredFunctions recovery mem textAddrRange info' (iterations + 1)

data ArchVals arch =
  ArchVals { archFunctions :: MS.MacawSymbolicArchFunctions arch
           , withArchEval :: forall a sym . (C.IsSymInterface sym) =>  sym  -> (MS.MacawArchEvalFn sym arch -> IO a) -> IO a
           , withArchConstraints :: forall a . ((C.IsSyntaxExtension (MS.MacawExt arch), MC.MemWidth (MC.ArchAddrWidth arch), MC.PrettyF (MC.ArchReg arch)) => a) -> a
           }

-- | A class to capture the architecture-specific information required to
-- perform block recovery and translation into a Crucible CFG.
--
-- For architectures that do not have a symbolic backend yet, have this function
-- return 'Nothing'.
class ArchInfo arch where
  archVals :: proxy arch -> Maybe (ArchVals arch)

toRegCFG :: forall arch ids s
          . (ArchBits arch)
         => C.HandleAllocator s
         -> MC.DiscoveryFunInfo arch ids
         -> Maybe (ST s (SCFG CR.SomeCFG arch))
toRegCFG halloc dfi = do
  archFns <- archFunctions <$> archVals (Proxy @arch)
  -- We only support statically linked binaries right now, so we don't have
  -- to deal with segments
  let memBaseVarMap = M.empty
  let nmTxt = T.decodeUtf8With T.lenientDecode (MC.discoveredFunName dfi)
  let nm = C.functionNameFromText nmTxt
  let posFn addr = C.BinaryPos nmTxt (maybe 0 fromIntegral (MC.msegAddr addr))
  return (MS.mkFunRegCFG archFns halloc memBaseVarMap nm posFn dfi)

toCFG :: forall arch
       . (ArchBits arch)
      => SymbolicRegCFG arch
      -> Maybe (IO (SCFG C.SomeCFG arch))
toCFG symRegCFG = do
  archFns <- archFunctions <$> archVals (Proxy @arch)
  return $ do
    regCFG <- getSymbolicRegCFG symRegCFG -- this is why we're in IO, not ST
    return $ MS.crucGenArchConstraints archFns $ MS.toCoreCFG archFns regCFG

cfgFromAddrsWith :: (ArchBits arch)
                 => Recovery arch
                 -> MC.Memory (MC.ArchAddrWidth arch)
                 -> (ConcreteAddress arch, ConcreteAddress arch)
                 -> MC.AddrSymMap (MC.ArchAddrWidth arch)
                 -> [MC.ArchSegmentOff arch]
                 -> [(MC.ArchSegmentOff arch, MC.ArchSegmentOff arch)]
                 -> IO (MC.DiscoveryState arch)
cfgFromAddrsWith recovery mem textAddrRange symbols initAddrs memWords = do
  let s1 = MC.markAddrsAsFunction MC.InitAddr initAddrs s0
  s2 <- analyzeDiscoveredFunctions recovery mem textAddrRange s1 0
  let s3 = MC.exploreMemPointers memWords s2
  analyzeDiscoveredFunctions recovery mem textAddrRange s3 0
  where
    s0 = MC.emptyDiscoveryState mem symbols (recoveryArchInfo recovery)

-- There can be overlapping blocks
--
-- We deduplicate identical blocks in this function.  We deal with overlapping
-- blocks by just never instrumenting them.
accumulateBlocks :: (MC.MemWidth (MC.ArchAddrWidth arch))
                 => M.Map (MC.ArchSegmentOff arch) (PU.Some (MC.ParsedBlock arch))
                 -> PU.Some (MC.ParsedBlock arch)
                 -> M.Map (MC.ArchSegmentOff arch) (PU.Some (MC.ParsedBlock arch))
accumulateBlocks m (PU.Some pb0)
  | Just (PU.Some pb) <- M.lookup (MC.pblockAddr pb0) m
  , MC.blockSize pb0 == MC.blockSize pb = m
  | otherwise = M.insert (MC.pblockAddr pb0) (PU.Some pb0) m

addrInRange :: (MC.MemWidth (MC.ArchAddrWidth arch))
            => MC.Memory (MC.ArchAddrWidth arch)
            -> (ConcreteAddress arch, ConcreteAddress arch)
            -> MC.ArchSegmentOff arch
            -> Bool
addrInRange mem (textStart, textEnd) addr = fromMaybe False $ do
  absAddr <- MC.msegAddr addr
  soStart <- MC.msegAddr =<< concreteAsSegmentOff mem textStart
  soEnd <- MC.msegAddr =<< concreteAsSegmentOff mem textEnd
  return (absAddr >= soStart && absAddr < soEnd)

blockInfo :: (ArchBits arch)
          => Recovery arch
          -> MC.Memory (MC.RegAddrWidth (MC.ArchReg arch))
          -> (ConcreteAddress arch, ConcreteAddress arch)
          -> MC.DiscoveryState arch
          -> IO (BlockInfo arch)
blockInfo recovery mem textAddrRange di = do
  let blockBuilder = buildBlock (recoveryDis recovery) mem
  let macawBlocks = F.foldl' accumulateBlocks M.empty [ PU.Some pb
                                                      | PU.Some dfi <- validFuncs
                                                      , pb <- M.elems (dfi L.^. MC.parsedBlocks)
                                                      , addrInRange mem textAddrRange (MC.pblockAddr pb)
                                                      ]
  blocks <- catMaybes <$> mapM blockBuilder (M.elems macawBlocks)
  let addBlock m b = M.insert (basicBlockAddress b) b m
  let blockIndex = F.foldl' addBlock M.empty blocks
  let funcBlocks = M.fromList [ (funcAddr, mapMaybe (\a -> M.lookup a blockIndex) blockAddrs)
                              | PU.Some dfi <- validFuncs
                              , Just funcAddr <- [concreteFromSegmentOff mem (MC.discoveredFunAddr dfi)]
                              , let blockAddrs = mapMaybe (concreteFromSegmentOff mem) (M.keys (dfi L.^. MC.parsedBlocks))
                              ]
  mcfgs <- T.forM validFuncs $ \(PU.Some dfi) -> do
    regIor <- IO.newIORef Nothing
    cfgIor <- IO.newIORef Nothing
    fromMaybe (return Nothing) $ do
      guard (not (isIncompleteFunction dfi))
      funcAddr <- concreteFromSegmentOff mem (MC.discoveredFunAddr dfi)
      regCFGGen <- toRegCFG (recoveryHandleAllocator recovery) dfi
      let regCFG = SymbolicRegCFG (Cached regIor (stToIO regCFGGen))
      cfgGen <- toCFG regCFG
      let cfg = SymbolicCFG (Cached cfgIor cfgGen)
      return (return (Just (funcAddr, (cfg, regCFG))))

  let cfgPairs = M.fromList (catMaybes mcfgs)

  return BlockInfo { biBlocks = blocks
                   , biFunctionEntries = mapMaybe (concreteFromSegmentOff mem) funcEntries
                   , biFunctionBlocks = funcBlocks
                   , biDiscoveryFunInfo = infos
                   , biIncomplete = indexIncompleteBlocks mem infos
                   , biCFG = M.map fst cfgPairs
                   , biRegCFG = M.map snd cfgPairs
                   , biOverlap = blockRegions mem di
                   }
  where
    validFuncs = M.elems (di L.^. MC.funInfo)
    funcEntries = [ MC.discoveredFunAddr dfi
                  | PU.Some dfi <- validFuncs
                  ]
    infos = M.fromList [ (concAddr, val)
                       | (segOff, val) <- M.toList (di L.^. MC.funInfo)
                       , Just concAddr <- return (concreteFromSegmentOff mem segOff)
                       ]

data Recovery arch =
  Recovery { recoveryISA :: ISA arch
           , recoveryDis :: forall m . (C.MonadThrow m) => B.ByteString -> m (Int, Instruction arch ())
           , recoveryArchInfo :: MC.ArchitectureInfo arch
           , recoveryHandleAllocator :: C.HandleAllocator RealWorld
           , recoveryBlockCallback :: Maybe (MC.ArchSegmentOff arch -> ST RealWorld ())
           , recoveryFuncCallback :: Maybe (Int, MC.ArchSegmentOff arch -> BlockInfo arch -> IO ())
           }

-- | Use macaw to discover code in a binary
--
-- > recoverBlocks recovery mem symmap entries textAddrRange
--
-- Performs code discover for a given memory image (@mem@) from a set of entry
-- points (@entries@).  Recovered functions are mapped to symbol table entries
-- (@symmap@) if possible.
--
-- The @textAddrRange@ parameter is used to constrain code recovery to a desired
-- range (usually the text section of the binary).  Macaw aggressively explores
-- all addresses encountered that live in an executable segment of memory.  This
-- is normally fine and a decent heuristic for dealing with code only reachable
-- via indirect call; however, on PowerPC it is problematic, as the Table of
-- Contents (TOC) is mapped in executable memory.  This causes macaw to decode
-- much of the TOC as code, most of which is not really valid code.  The code
-- that attempts to make code relocatable (the symbolization phase) fails badly
-- in this case.  To avoid these failures, we constrain our code recovery to the
-- text section.
recoverBlocks :: (ArchBits arch)
              => Recovery arch
              -> MC.Memory (MC.ArchAddrWidth arch)
              -> SymbolMap arch
              -> NEL.NonEmpty (MC.MemSegmentOff (MC.ArchAddrWidth arch))
              -> (ConcreteAddress arch, ConcreteAddress arch)
              -> IO (BlockInfo arch)
recoverBlocks recovery mem symmap entries textAddrRange = do
  sam <- toMacawSymbolMap mem symmap
  di <- cfgFromAddrsWith recovery mem textAddrRange sam (F.toList entries) []
  blockInfo recovery mem textAddrRange di

toMacawSymbolMap :: (MC.MemWidth (MC.ArchAddrWidth arch)) => MC.Memory (MC.ArchAddrWidth arch) -> SymbolMap arch -> IO (MC.AddrSymMap (MC.ArchAddrWidth arch))
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
--
-- FIXME: Keep a record of which macaw blocks we can't translate (for whatever reason)
buildBlock :: (L.HasCallStack, MC.MemWidth (MC.ArchAddrWidth arch), C.MonadThrow m)
           => (B.ByteString -> Maybe (Int, Instruction arch ()))
           -- ^ The function to pull a single instruction off of the
           -- byte stream; it returns the number of bytes consumed and
           -- the new instruction.
           -> MC.Memory (MC.ArchAddrWidth arch)
           -> PU.Some (MC.ParsedBlock arch)
           -- ^ The macaw block to re-disassemble
           -> m (Maybe (ConcreteBlock arch))
buildBlock dis1 mem (PU.Some pb)
  | Just concAddr <- concreteFromSegmentOff mem segAddr = do
      case MC.addrContentsAfter mem (MC.relativeSegmentAddr segAddr) of
        Left err -> C.throwM (MemoryError err)
        Right [MC.ByteRegion bs] -> do
          let stopAddr = concAddr `addressAddOffset` fromIntegral (MC.blockSize pb)
          Just <$> go concAddr concAddr stopAddr bs []
        _ -> C.throwM (NoByteRegionAtAddress (MC.relativeSegmentAddr segAddr))
  | otherwise = return Nothing
  where
    segAddr = MC.pblockAddr pb
    addOff       = addressAddOffset
    go blockAbsAddr insnAddr stopAddr bs insns = do
      case dis1 bs of
        -- Actually, we should probably never hit this case.  We
        -- should have hit a terminator instruction or end of block
        -- before running out of bytes...
        Nothing -> return BasicBlock { basicBlockAddress = blockAbsAddr
                                     , basicBlockInstructions = reverse insns
                                     }
        Just (bytesRead, i)
          -- We have parsed an instruction that crosses a block boundary. We
          -- should probably give up -- this executable is too wonky.
          | nextAddr > stopAddr -> do
            C.throwM (OverlappingBlocks insnAddr)

          -- The next instruction we would decode starts another
          -- block, OR the instruction we just decoded is a
          -- terminator, so end the block and stop decoding
          | nextAddr == stopAddr -> do
            return BasicBlock { basicBlockAddress      = blockAbsAddr
                              , basicBlockInstructions = reverse (i : insns)
                              }

          -- Otherwise, we just keep decoding
          | otherwise -> go blockAbsAddr nextAddr stopAddr (B.drop bytesRead bs) (i : insns)
          where
          nextAddr = insnAddr `addOff` fromIntegral bytesRead

-- | Collect the set of basic block addresses of blocks that are contained in incomplete functions
--
-- A function is incomplete if it contains an error terminator (translation
-- error or classify failure).  We can't rewrite these blocks, as we don't know
-- if we have a safe view of them.  Rewriting them could introduce runtime
-- failures.
indexIncompleteBlocks :: (MC.MemWidth (MC.ArchAddrWidth arch))
                      => MC.Memory (MC.ArchAddrWidth arch)
                      -> M.Map (ConcreteAddress arch) (PU.Some (MC.DiscoveryFunInfo arch))
                      -> S.Set (ConcreteAddress arch)
indexIncompleteBlocks mem = foldr (addFunInfoIfIncomplete mem) S.empty

addFunInfoIfIncomplete :: (MC.MemWidth (MC.ArchAddrWidth arch))
                       => MC.Memory (MC.ArchAddrWidth arch)
                       -> PU.Some (MC.DiscoveryFunInfo arch)
                       -> S.Set (ConcreteAddress arch)
                       -> S.Set (ConcreteAddress arch)
addFunInfoIfIncomplete mem (PU.Some fi) s
  | isIncompleteFunction fi = S.union s (S.fromList blockAddrs)
  | otherwise = s
  where
    pbs = fi L.^. MC.parsedBlocks
    blockAddrs = mapMaybe (concreteFromSegmentOff mem) (M.keys pbs)

isIncompleteFunction :: (MC.MemWidth (MC.ArchAddrWidth arch)) => MC.DiscoveryFunInfo arch ids -> Bool
isIncompleteFunction fi =
  any isIncomplete (M.elems (fi L.^. MC.parsedBlocks))

isIncomplete :: (MC.MemWidth (MC.ArchAddrWidth arch)) => MC.ParsedBlock arch ids -> Bool
isIncomplete pb =
  case MC.stmtsTerm (MC.blockStatementList pb) of
    MC.ParsedTranslateError {} -> True
    MC.ClassifyFailure {} -> True
    MC.ParsedArchTermStmt {} -> False
    MC.ParsedIte {} -> False
    MC.ParsedReturn {} -> False
    MC.ParsedLookupTable {} -> False
    MC.ParsedJump {} -> False
    MC.ParsedCall {} -> False

{- Note [Unaligned Instructions]

We still need to raise an error if we see a jump into an unaligned
instruction.  It is technically perfectly possible, but we don't want
to support or encourage it.

-}

{- Note [CrucibleCFG]

We construct Crucible CFGs only for functions that are complete (i.e., for which
all control flow is resolved).  Furthermore, we require the appropriate symbolic
backend.  For architectures that lack a symbolic backend, there will be no
entries in the CFG map.

-}
