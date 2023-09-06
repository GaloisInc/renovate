{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The interface for 'BasicBlock' recovery
module Renovate.Recovery (
  Recovery(..),
  recoverBlocks,
  BlockInfo(..),
  SymbolicCFG,
  SymbolicRegCFG,
  SCFG,
  getSymbolicCFG,
  getSymbolicRegCFG,
  isIncompleteBlockAddress,
  isIncompleteFunction,
  isIncompleteBlock,
  numBlockRegions
  ) where

import Debug.Trace (traceM, trace)
import Numeric (showHex)

import qualified Control.Lens as L
import           Control.Monad ( guard )
import qualified Control.Monad.Catch as C
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.IO.Class ( liftIO )
import qualified Control.Monad.IO.Unlift as MIU
import qualified Control.Monad.Reader as CMR
import qualified Data.ByteString as B
import           Data.Either ( partitionEithers )
import qualified Data.Foldable as F
import qualified Data.IORef as IO
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import           Data.Maybe ( catMaybes, fromMaybe, isJust, mapMaybe )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Set as S
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Traversable as T
import           GHC.Stack ( HasCallStack )
import           GHC.TypeLits
import qualified Lumberjack as LJ

import qualified Data.Macaw.Architecture.Info as MC
import qualified Data.Macaw.BinaryLoader as MBL
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.Discovery as MC
import qualified Data.Macaw.Refinement as MR
import qualified Data.Macaw.Symbolic as MS
import qualified Data.Parameterized.Context as Ctx
import qualified Data.Parameterized.Some as PU
import qualified Lang.Crucible.CFG.Core as C
import qualified Lang.Crucible.CFG.Reg as CR
import qualified Lang.Crucible.FunctionHandle as C
import qualified Lang.Crucible.LLVM.MemModel as LCLM
import qualified What4.FunctionName as C
import qualified What4.ProgramLoc as C

import           Renovate.Core.Address
import           Renovate.Core.BasicBlock
import qualified Renovate.Core.Diagnostic as RCD
import qualified Renovate.Core.Exception as RCE
import qualified Renovate.Core.Instruction as RCI
import           Renovate.ISA
import           Renovate.Recovery.Overlap
import           Renovate.Recovery.SymbolMap ( SymbolMap, toMacawSymbolMap )
import Data.Macaw.Discovery (DiscoveryFunInfo)

data Cached a = Cached (IO.IORef (Maybe a)) (IO a)
type SCFG f arch = f (MS.MacawExt arch) (Ctx.EmptyCtx Ctx.::> MS.ArchRegStruct arch) (MS.ArchRegStruct arch)
newtype SymbolicCFG arch = SymbolicCFG (Cached (SCFG C.SomeCFG arch))
newtype SymbolicRegCFG arch = SymbolicRegCFG (Cached (SCFG CR.SomeCFG arch))

-- | Construct or return the cached symbolic CFG
--
-- We have this representation to let us lazily construct CFGs, as we won't
-- usually need all of them
getSymbolicCFG :: SymbolicCFG arch -> IO (SCFG C.SomeCFG arch)
getSymbolicCFG (SymbolicCFG cached) = getCached cached

-- | Construct or return the cached symbolic registerized CFG
getSymbolicRegCFG :: SymbolicRegCFG arch -> IO (SCFG CR.SomeCFG arch)
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
  -- ^ All blocks found
  , biFunctionEntries  :: [ConcreteAddress arch]
  -- ^ All known function addresses
  , biFunctions        :: M.Map (ConcreteAddress arch) ([ConcreteBlock arch], PU.Some (MC.DiscoveryFunInfo arch))
  -- ^ Map from a function address to the set of all blocks in that function,
  -- along with the other information discovered by macaw for that function.
  , biIncomplete       :: S.Set (ConcreteAddress arch)
  -- ^ The set of blocks that reside in incomplete functions (i.e.,
  -- functions for which we cannot find all of the code).  Note that
  -- this is the set of *all* addresses of *blocks*; these are not
  -- function addresses, and these are all blocks in a function which
  -- is incomplete (because attempting to reason about or rewrite
  -- these blocks could be dangerous because the incomplete blocks
  -- might jump back into the middle known blocks, therefore the known
  -- blocks cannot be considered to be complete either).
  , biCFG              :: M.Map (ConcreteAddress arch) (SymbolicCFG arch)
  -- ^ The Crucible CFG for each function (if possible to
  -- construct). Crucible CFGs are only constructed for functions that
  -- are complete (i.e., for which all control flow is resolved).
  -- Furthermore, the appropriate symbolic backend is required.  For
  -- architectures that lack a symbolic backend, there will be
  -- no entries in the CFG map.
  , biRegCFG           :: M.Map (ConcreteAddress arch) (SymbolicRegCFG arch)
  , biOverlap          :: BlockRegions arch
  -- ^ A structure that lets us determine which blocks in the program overlap
  -- other blocks in the program (so that we can avoid ever trying to rewrite them)
  , biDiscoveryInfo :: MC.DiscoveryState arch
  }

isIncompleteBlockAddress :: BlockInfo arch -> ConcreteAddress arch -> Bool
isIncompleteBlockAddress bi a = S.member a (biIncomplete bi)

analyzeDiscoveredFunctions :: (MS.SymArchConstraints arch)
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
      (info', C.Some fnInfo) <- return $ MC.analyzeFunction addr rsn info
      putStrLn $ "analyzeDiscoveredFunctions: addr=" ++ show addr ++ " sym:" ++ show (MC.discoveredFunSymbol fnInfo)
      case recoveryFuncFilter recovery addr (Just (MC.discoveredFunName fnInfo)) of
        False -> do
          putStrLn "  dropped"
          info'' <- return $ info L.& MC.unexploredFunctions L.%~ (M.delete addr)
          fnInfo' <- return $ fnInfo L.& MC.parsedBlocks L.%~ (\_ -> M.empty)
          info''' <- return $ info'' L.& MC.funInfo L.%~ M.insert addr (C.Some fnInfo')
          analyzeDiscoveredFunctions recovery mem textAddrRange info''' (iterations + 1)
        True -> do
          case recoveryFuncCallback recovery of
            Just (freq, fcb)
              | iterations `mod` freq == 0 -> do
                  bi <- blockInfo recovery mem textAddrRange info'
                  fcb addr bi
            _ -> return ()
          analyzeDiscoveredFunctions recovery mem textAddrRange info' (iterations + 1)

toRegCFG :: forall arch ids
          . (MS.SymArchConstraints arch)
         => C.HandleAllocator
         -> MC.DiscoveryFunInfo arch ids
         -> Maybe (IO (SCFG CR.SomeCFG arch))
toRegCFG halloc dfi = do
  archFns <- MS.archFunctions <$> MS.archVals (Proxy @arch) Nothing
  let nmTxt = T.decodeUtf8With T.lenientDecode (MC.discoveredFunName dfi)
  let nm = C.functionNameFromText nmTxt
  let posFn addr = C.BinaryPos nmTxt (maybe 0 fromIntegral (MC.segoffAsAbsoluteAddr addr))
  return (MS.mkFunRegCFG archFns halloc nm posFn dfi)

toCFG :: forall arch
       . (MS.SymArchConstraints arch)
      => SymbolicRegCFG arch
      -> Maybe (IO (SCFG C.SomeCFG arch))
toCFG symRegCFG = do
  archFns <- MS.archFunctions <$> MS.archVals (Proxy @arch) Nothing
  return $ do
    regCFG <- getSymbolicRegCFG symRegCFG -- this is why we're in IO, not ST
    return (MS.toCoreCFG archFns regCFG)

cfgFromAddrsWith :: (MS.SymArchConstraints arch)
                 => Recovery arch
                 -> MC.Memory (MC.ArchAddrWidth arch)
                 -> (ConcreteAddress arch, ConcreteAddress arch)
                 -> MC.AddrSymMap (MC.ArchAddrWidth arch)
                 -> [MC.ArchSegmentOff arch]
                 -> IO (MC.DiscoveryState arch)
cfgFromAddrsWith recovery mem textAddrRange symbols initAddrs = do
  let s1 = MC.markAddrsAsFunction MC.InitAddr initAddrs s0
  s2 <- analyzeDiscoveredFunctions recovery mem textAddrRange s1 0
  analyzeDiscoveredFunctions recovery mem textAddrRange s2 0
  where
    s0 = MC.emptyDiscoveryState mem symbols (recoveryArchInfo recovery)

-- There can be overlapping blocks
--
-- We deduplicate identical blocks in this function.  We deal with overlapping
-- blocks by just never instrumenting them.
accumulateBlocks :: (MS.SymArchConstraints arch) -- MC.MemWidth (MC.ArchAddrWidth arch))
                 => M.Map (MC.ArchSegmentOff arch) (MC.ArchSegmentOff arch, (PU.Some (MC.ParsedBlock arch)))
                 -> (MC.ArchSegmentOff arch, PU.Some (MC.ParsedBlock arch))
                 -> M.Map (MC.ArchSegmentOff arch) (MC.ArchSegmentOff arch, (PU.Some (MC.ParsedBlock arch)))
accumulateBlocks m v@(faddr0, (PU.Some pb0)) = case M.lookup (MC.pblockAddr pb0) m of
  Just (faddr, (PU.Some pb)) ->
    case MC.blockSize pb0 == MC.blockSize pb && faddr0 == faddr of
      True  -> m
      False -> M.insert (MC.pblockAddr pb0) v m
  Nothing -> M.insert (MC.pblockAddr pb0) v m

addrInRange :: (MC.MemWidth (MC.ArchAddrWidth arch))
            => (ConcreteAddress arch, ConcreteAddress arch)
            -> MC.ArchSegmentOff arch
            -> Bool
addrInRange (textStart, textEnd) addr = fromMaybe False $ do
  absAddr <- MC.segoffAsAbsoluteAddr addr
  let soStart = absoluteAddress textStart
  let soEnd = absoluteAddress textEnd
  return (absAddr >= soStart && absAddr < soEnd)


-- | Clear the least significant bit of a segment offset.
clearSegOffLeastBit :: MC.Memory w
                    -> MC.MemSegmentOff w
                    -> MC.MemSegmentOff w
clearSegOffLeastBit mem addr = case MC.asSegmentOff mem (MC.clearAddrLeastBit (MC.segoffAddr addr)) of
  Just addr' -> addr'
  Nothing -> error "PANIC: clearSegOffLeastBit (Recovery.hs)"


blockInfo :: forall arch
           . (MS.SymArchConstraints arch)
          => Recovery arch
          -> MC.Memory (MC.RegAddrWidth (MC.ArchReg arch))
          -> (ConcreteAddress arch, ConcreteAddress arch)
          -> MC.DiscoveryState arch
          -> IO (BlockInfo arch)
blockInfo recovery mem textAddrRange di = do
  let blockBuilder = buildBlock (recoveryDis recovery) (recoveryAsm recovery) mem
  let macawBlocks = F.foldl' accumulateBlocks M.empty [ (faddr, PU.Some pb)
                                                      | PU.Some dfi <- validFuncs
                                                      , _pb <- M.elems (dfi L.^. MC.parsedBlocks)
                                                      , addrInRange textAddrRange (MC.pblockAddr _pb)
                                                      , let _faddr = MC.discoveredFunAddr dfi
                                                      , let pb = _pb { MC.pblockAddr = clearSegOffLeastBit mem (MC.pblockAddr _pb) }
                                                      , let faddr = clearSegOffLeastBit mem _faddr
                                                      -- , even (MC.segoffOffset (MC.pblockAddr pb))
                                                      ]
  let blockStarts = M.fromList [ (baddr, baddr `addressAddOffset` fromIntegral (MC.blockSize b))
                               | (archSegOff, (_, PU.Some b)) <- M.toList macawBlocks
                               , Just baddr <- return (concreteFromSegmentOff mem archSegOff)
                               , MC.blockSize b /= 0
                               , even (absoluteAddress baddr)
                               ]

  traceM (show blockStarts)

  -- We collect not only the blocks, but also the addresses of functions
  -- containing untranslatable blocks so that they can be marked as incomplete.
  (incomp, blocks) <- partitionEithers <$> mapM (blockBuilder blockStarts) (M.elems macawBlocks)
  let addBlock m b = M.insert (concreteBlockAddress b) b m
  let blockIndex = F.foldl' addBlock M.empty blocks
  let funcBlocks = M.fromList [ (funcAddr, (mapMaybe (\a -> M.lookup a blockIndex) blockAddrs, PU.Some dfi))
                              | PU.Some dfi <- validFuncs
                              , Just funcAddr <- [concreteFromSegmentOff mem (MC.discoveredFunAddr dfi)]
                              , let blockAddrs = mapMaybe (concreteFromSegmentOff mem) (M.keys (dfi L.^. MC.parsedBlocks))
                              ]

  -- F.forM_ validFuncs $ \(PU.Some dfi) -> do
  --   let addr = MC.discoveredFunAddr dfi
  --   putStrLn ("addr = " ++ show addr)
  --   let seg = MC.segoffSegment addr
  --   putStrLn ("  segment size = " ++ show (MC.segmentSize seg))
  --   putStrLn ("  segoff = " ++ show (MC.segoffOffset addr))
  --   print (MC.segoffSegment (MC.discoveredFunAddr dfi))
  --   print (PP.pretty dfi)
  --   F.forM_ (M.elems (dfi L.^. MC.parsedBlocks)) $ \pb -> do
  --     putStrLn ("Reason: " ++ show (MC.blockReason pb))

  mcfgs <- T.forM validFuncs $ \(PU.Some dfi) -> do
    regIor <- IO.newIORef Nothing
    cfgIor <- IO.newIORef Nothing
    fromMaybe (return Nothing) $ do
      guard (not (isIncompleteFunction dfi))
      funcAddr <- concreteFromSegmentOff mem (MC.discoveredFunAddr dfi)
      regCFGGen <- toRegCFG (recoveryHandleAllocator recovery) dfi
      let regCFG = SymbolicRegCFG (Cached regIor regCFGGen)
      cfgGen <- toCFG regCFG
      let cfg = SymbolicCFG (Cached cfgIor cfgGen)
      return (return (Just (funcAddr, (cfg, regCFG))))

  let cfgPairs = M.fromList (catMaybes mcfgs)

  return BlockInfo { biBlocks = blocks
                   , biFunctionEntries = mapMaybe (concreteFromSegmentOff mem) funcEntries
                   , biFunctions = funcBlocks
                   , biIncomplete = indexIncompleteBlocks (S.fromList incomp) mem infos
                   , biCFG = M.map fst cfgPairs
                   , biRegCFG = M.map snd cfgPairs
                   , biOverlap = blockRegions mem di
                   , biDiscoveryInfo = di
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
           , recoveryDis :: forall m ids . (C.MonadThrow m) => MC.ParsedBlock arch ids -> ConcreteAddress arch -> ConcreteAddress arch -> B.ByteString -> m (ConcreteBlock arch)
           , recoveryAsm :: forall m tp . (C.MonadThrow m) => RCI.Instruction arch tp () -> m B.ByteString
           , recoveryArchInfo :: MC.ArchitectureInfo arch
           , recoveryHandleAllocator :: C.HandleAllocator
           , recoveryFuncCallback :: Maybe (Int, MC.ArchSegmentOff arch -> BlockInfo arch -> IO ())
           , recoveryFuncFilter :: MC.ArchSegmentOff arch -> Maybe B.ByteString -> Bool
           , recoveryRefinement :: Maybe MR.RefinementConfig
           }

reportDiscoveryFailures
  :: (MC.MemWidth (MC.ArchAddrWidth arch))
  => LJ.LogAction IO (RCD.Diagnostic l)
  -> PU.Some (MC.DiscoveryFunInfo arch)
  -> IO ()
reportDiscoveryFailures logAction (PU.Some dfi) =
  mapM_ reportBlockDiscoveryFailures (M.toList (dfi L.^. MC.parsedBlocks))
  where
    reportBlockDiscoveryFailures (addr, pb) =
      case MC.pblockTermStmt pb of
        MC.ClassifyFailure _ msgs ->
          LJ.writeLog logAction (RCD.RecoveryDiagnostic (RCD.ClassificationFailure addr msgs))
        MC.ParsedTranslateError msg ->
          LJ.writeLog logAction (RCD.RecoveryDiagnostic (RCD.TranslationError addr msg))
        _ -> return ()

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
recoverBlocks
  :: (MS.SymArchConstraints arch, 16 <= MC.ArchAddrWidth arch)
  => LJ.LogAction IO (RCD.Diagnostic lm)
  -> Recovery arch
  -> MBL.LoadedBinary arch binFmt
  -> SymbolMap arch
  -> NEL.NonEmpty (MC.MemSegmentOff (MC.ArchAddrWidth arch))
  -> (ConcreteAddress arch, ConcreteAddress arch)
  -> IO (BlockInfo arch)
recoverBlocks logAction recovery loadedBinary symmap entries textAddrRange = do
  putStrLn "1!!"
  let mem = MBL.memoryImage loadedBinary
  sam <- toMacawSymbolMap mem symmap
  putStrLn "2!!"
  let entries' = filter (\addr -> (M.lookup addr sam) == Just "main") (F.toList entries)
  di <- cfgFromAddrsWith recovery mem textAddrRange sam entries'
  putStrLn "3!!"
  -- If the caller requested refinement, call refinement in a loop until nothing changes
  di' <- case recoveryRefinement recovery of
    Nothing -> putStrLn "3a!!" >> return di
    Just refineCfg -> do
      rc <- MR.defaultRefinementContext refineCfg loadedBinary
      putStrLn "3b!!"
      refineDiscoveryInfo logAction rc di
  putStrLn "4!!"
  binfo <- blockInfo recovery mem textAddrRange di'
  putStrLn "5!!"
  mapM_ (reportDiscoveryFailures logAction) (M.elems (biDiscoveryInfo binfo L.^. MC.funInfo))
  putStrLn "6!!"
  return binfo

newtype RefineM arch a = RefineM { unRefineM :: CMR.ReaderT (LJ.LogAction (RefineM arch) (MR.RefinementLog arch)) IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MIU.MonadUnliftIO
           , C.MonadThrow
           , CMR.MonadReader (LJ.LogAction (RefineM arch) (MR.RefinementLog arch))
           )

refineLog :: (MC.MemWidth (MC.ArchAddrWidth arch))
          => LJ.LogAction IO (RCD.Diagnostic lm)
          -> LJ.LogAction (RefineM arch) (MR.RefinementLog arch)
refineLog logAction =
  LJ.LogAction $ \msg -> do
    liftIO (LJ.writeLog logAction (RCD.RecoveryDiagnostic (RCD.RefinementLog msg)))

runRefineM
  :: forall lm arch a
   . (MC.MemWidth (MC.ArchAddrWidth arch))
  => LJ.LogAction IO (RCD.Diagnostic lm)
  -> RefineM arch a
  -> IO a
runRefineM logAction a = CMR.runReaderT (unRefineM a) (refineLog logAction)

instance LJ.HasLog (MR.RefinementLog arch) (RefineM arch) where
  getLogAction = CMR.ask

refineDiscoveryInfo :: forall arch lm
                     . ( 16 <= MC.ArchAddrWidth arch
                       , MS.SymArchConstraints arch
                       )
                    => LJ.LogAction IO (RCD.Diagnostic lm)
                    -> MR.RefinementContext arch
                    -> MC.DiscoveryState arch
                    -> IO (MC.DiscoveryState arch)
refineDiscoveryInfo logAction rc = go mempty
  where
    go findings0 s0 = do
      -- NOTE: We are currently throwing away all of the diagnostics from
      -- macaw-refinement.  We could collect them in an @MVar (Seq msg)@ or
      -- similar if we wanted them
      let ?memOpts = LCLM.defaultMemOptions
      (s1, findings1) <- runRefineM @_ @arch logAction $ MR.refineDiscovery rc findings0 s0
      case findings0 == findings1 of
        True -> return s1
        False -> go findings1 s1

canAssemble :: (t -> Maybe a) -> t -> Bool
canAssemble asm1 i =
  isJust (asm1 i)

-- | Compute the address we should stop assembling at for the block
--
-- This is more complicated than we would like - ideally, we would treat macaw
-- blocks as ground truth in this.  Unfortunately, there are a number of cases
-- where macaw returns overlapping blocks that we do not want to have in
-- renovate:
--
-- * If macaw builds a block, but later decides to split it, it doesn't
--   remove/replace the first block.  It adds a new block that is the second
--   portion of the block, but the original block is unmodified and overlaps.
--   In this case, it might even be the case that the blocks coming back from
--   macaw overlap but do *not* share a suffix
--
-- * Some code (usually hand-written) legitimately creates blocks that must
--   overlap where they share a suffix.  One case here is where x86 code has an
--   instruction with a lock prefix for atomic memory updates, but where that
--   prefix is conditionally not necessary.  Sometimes, such code will jump past
--   the lock prefix into the middle of an instruction.
--
-- To deal with this, we look at the next block after the current one.  If the
-- next block shares a suffix with the current block, we treat them as
-- necessarily overlapping and just make one block to cover both regions.
-- Otherwise, we stop disassembling at the start of the next block (assuming
-- that macaw *should* have split the current block, but did not).
--
-- Note that macaw can identify some code as trivially unreachable, leaving gaps
-- in discovered code.  The second case in the multiway if covers that case.
--
-- Note also that renovate makes the assumption that all renovate blocks either
-- do not overlap OR, if they do overlap, share a suffix (i.e., end at the same
-- instruction).
blockStopAddress :: (MC.MemWidth (MC.ArchAddrWidth arch))
                 => M.Map (ConcreteAddress arch) (ConcreteAddress arch)
                 -- ^ Block starts mapped to block ends (derived from macaw discovery info)
                 -> MC.ParsedBlock arch s
                 -- ^ Block being translated
                 -> ConcreteAddress arch
                 -- ^ Block start address
                 -> ConcreteAddress arch
blockStopAddress blockStarts pb startAddr
  | Just (nextStart, nextEnd) <- M.lookupGT startAddr blockStarts =
    if | nextEnd == naturalEnd -> naturalEnd
       | naturalEnd < nextStart -> naturalEnd
       | otherwise -> nextStart
  | otherwise = naturalEnd
  where
    naturalEnd = startAddr `addressAddOffset` fromIntegral (MC.blockSize pb)

-- | Build our representation of a basic block from a provided block
-- start address
--
-- The block starts are obtained from Macaw.  We disassemble from the
-- start address until we hit a terminator instruction or the start of
-- another basic block.
--
-- For each block, it is either translated (Right), or we report the address of
-- the function containing the untranslatable block (Left).  We need this list
-- to mark functions as incomplete.
buildBlock :: (HasCallStack, MC.MemWidth (MC.ArchAddrWidth arch), C.MonadThrow m, MS.SymArchConstraints arch)
           => (forall ids . MC.ParsedBlock arch ids -> ConcreteAddress arch -> ConcreteAddress arch -> B.ByteString -> Either C.SomeException (ConcreteBlock arch))
           -- ^ A function to disassemble an entire block at once (up to the
           -- requested number of bytes)
           -> (forall tp . RCI.Instruction arch tp () -> Maybe B.ByteString)
           -- ^ The function to pull a single instruction off of the
           -- byte stream; it returns the number of bytes consumed and
           -- the new instruction.
           -> MC.Memory (MC.ArchAddrWidth arch)
           -> M.Map (ConcreteAddress arch) (ConcreteAddress arch)
           -- ^ Block starts
           -> (MC.ArchSegmentOff arch, PU.Some (MC.ParsedBlock arch))
           -- ^ The macaw block to re-disassemble
           -> m (Either (MC.ArchSegmentOff arch) (ConcreteBlock arch))
buildBlock disBlock asm1 mem blockStarts (funcAddr, (PU.Some pb)) = do
  -- traceM ("PARSED BLOCK at: " ++ show (MC.pblockAddr pb) ++ "\n\nSTMTS:\n" ++ show (MC.pblockStmts pb) ++ "\n\nTERM STMT:\n" ++ show (MC.pblockTermStmt pb))
  if | MC.blockSize pb == 0 -> return (Left funcAddr)
     | isIncompleteBlock pb -> return (Left funcAddr)
     | Just concAddr <- concreteFromSegmentOff mem segAddr -> do
         case MC.addrContentsAfter mem (MC.segoffAddr segAddr) of
           Left err -> C.throwM (RCE.MemoryError segAddr err)
           Right [MC.ByteRegion bs] -> do
             let stopAddr = blockStopAddress blockStarts pb concAddr
             case disBlock pb concAddr stopAddr bs of
               Left err -> return (Left funcAddr)
               Right bb ->
                 -- If we can't re-assemble all of the instructions we have found,
                 -- pretend we never saw this block.  Note that the caller will have to
                 -- remember this to note that the function containing this block is
                 -- incomplete.
                 --
                 -- We do this to avoid re-assembly errors later, where we can't do
                 -- anything about it.
                 withConcreteInstructions bb $ \_repr insns -> do
                   case all (canAssemble asm1) insns of
                     True -> return (Right bb)
                     False -> return (Left funcAddr)
           _ -> C.throwM (RCE.NoByteRegionAtAddress (MC.segoffAddr segAddr))
     | otherwise -> return (Left funcAddr)
     where
       segAddr = MC.pblockAddr pb

-- | Collect the set of basic block addresses of blocks that are contained in incomplete functions
--
-- A function is incomplete if it contains an error terminator (translation
-- error or classify failure).  We can't rewrite these blocks, as we don't know
-- if we have a safe view of them.  Rewriting them could introduce runtime
-- failures.
--
-- We include an extra set of functions that are known to be incomplete due to
-- translation errors in renovate (vs translation errors in macaw).
indexIncompleteBlocks :: (MC.MemWidth (MC.ArchAddrWidth arch))
                      => S.Set (MC.ArchSegmentOff arch)
                      -> MC.Memory (MC.ArchAddrWidth arch)
                      -> M.Map (ConcreteAddress arch) (PU.Some (MC.DiscoveryFunInfo arch))
                      -> S.Set (ConcreteAddress arch)
indexIncompleteBlocks incompAddrs mem = foldr (addFunInfoIfIncomplete incompAddrs mem) S.empty

addFunInfoIfIncomplete :: (MC.MemWidth (MC.ArchAddrWidth arch))
                       => S.Set (MC.ArchSegmentOff arch)
                       -> MC.Memory (MC.ArchAddrWidth arch)
                       -> PU.Some (MC.DiscoveryFunInfo arch)
                       -> S.Set (ConcreteAddress arch)
                       -> S.Set (ConcreteAddress arch)
addFunInfoIfIncomplete incompAddrs mem (PU.Some fi) s
  | isIncompleteFunction fi || S.member (MC.discoveredFunAddr fi) incompAddrs =
    S.union s (S.fromList blockAddrs)
  | otherwise = s
  where
    pbs = fi L.^. MC.parsedBlocks
    blockAddrs = mapMaybe (concreteFromSegmentOff mem) (M.keys pbs)

-- | Use 'isIncompleteFunction' to determine if particular function
-- has been fully analyzed or if the analysis was incomplete for any
-- block in the function.
isIncompleteFunction :: (MC.MemWidth (MC.ArchAddrWidth arch)) => MC.DiscoveryFunInfo arch ids -> Bool
isIncompleteFunction fi =
  any isIncompleteBlock (M.elems (fi L.^. MC.parsedBlocks))

-- | Use 'isIncompleteBlock' to determine if a specific CFG block has
-- been fully analyzed or if the analysis was incomplete.  A @show $
-- pretty blk@ operation can be used to get a readable explanation of
-- why the analysis of @blk@ was incomplete.
isIncompleteBlock :: (MC.MemWidth (MC.ArchAddrWidth arch)) => MC.ParsedBlock arch ids -> Bool
isIncompleteBlock pb =
  case MC.pblockTermStmt pb of
    MC.ParsedTranslateError {} -> True
    MC.ClassifyFailure {} -> True
    MC.ParsedBranch {} -> False
    MC.ParsedArchTermStmt {} -> False
    MC.ParsedReturn {} -> False
    MC.ParsedLookupTable {} -> False
    MC.ParsedJump {} -> False
    MC.ParsedCall {} -> False
    MC.PLTStub {} -> False

{- Note [Unaligned Instructions]

We still need to raise an error if we see a jump into an unaligned
instruction.  It is technically perfectly possible, but we don't want
to support or encourage it.

-}

