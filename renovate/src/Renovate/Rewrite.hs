{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Renovate.Rewrite (
  RewriteM,
  RewriteInfo(..),
  BlockCFGIndex,
  mkRewriteEnv,
  runRewriteM,
  instrumentBlocks,
  lookupBlockCFG,
  lookupEntryAddress,
  getInjectedInstructions,
  getInjectedFunctions,
  newGlobalVar,
  getBlockIndex,
  getABI,
  getISA,
  InjectSymbolicInstructions(..),
  injectInstructions,
  injectFunction,
  recordRewrite,
  recordLogMsg,
  logDiagnostic,
  ) where

import qualified Control.Monad.Catch as X
import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Map.Strict as M
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Set as S
import qualified Data.Traversable as T
import           Data.Word ( Word32 )
import qualified Lumberjack as LJ

import qualified Data.Macaw.CFG as MM
import qualified Lang.Crucible.FunctionHandle as C

import qualified Renovate.ABI as ABI
import qualified Renovate.Analysis.FunctionRecovery as FR
import qualified Renovate.Core.Address as A
import qualified Renovate.Core.BasicBlock as B
import qualified Renovate.Core.Diagnostic as RCD
import qualified Renovate.Core.Instruction as RCI
import qualified Renovate.Core.Layout as RCL
import qualified Renovate.Core.Relocation as RCR
import qualified Renovate.ISA as ISA
import qualified Renovate.Recovery as RR
import           Renovate.Recovery.Overlap ( disjoint )
import qualified Renovate.Redirect.Symbolize as RS

-- | The state for the 'RewriteM' monad.
--
-- The @lm@ type is an arbitrary user controlled type that can be
-- logged during rewriting using 'recordLogMsg'.
data RewriteInfo arch =
  RewriteInfo { newGlobals :: M.Map String (A.ConcreteAddress arch)
              -- ^ A mapping of names to globals allocated in a new data
              -- section.  The names are user-provided and must be unique.
              , nextGlobalAddress :: A.ConcreteAddress arch
              -- ^ The next address that will be handed out for a new global
              -- variable.
              , symAddrAlloc :: RS.SymbolicAddressAllocator arch
              -- ^ An allocator for symbolic addresses for blocks, used for the function
              -- injection API provided by 'RewriteM'
              , injectedFunctions :: M.Map (A.SymbolicAddress arch) (String, BS.ByteString)
              -- ^ Functions injected (most likely during the setup phase, but not necessarily)
              , injectedInstructions :: M.Map (A.SymbolicAddress arch) (String, InjectSymbolicInstructions arch)
              -- ^ Bundles of instructions that are to be injected into the program
              --
              -- This is a method for injecting instructions that need to be
              -- concretized (i.e., that cannot be reduced to a 'BS.ByteString'
              -- by the caller)
              }

data RewriteEnv lm arch = RewriteEnv
  { envBlockCFGIndex :: BlockCFGIndex arch
  -- ^ A map of block addresses to the set of CFGs that
  -- contains them (if any). Note that a single block may
  -- be contained in multiple CFGs. Indeed, the
  -- @embrittle-examples.sh@ in @sfe@ include such CFGs.
  , envEntryAddress :: A.ConcreteAddress arch
  -- ^ The address of the entry point of the program
  , envMemory :: MM.Memory (MM.ArchAddrWidth arch)
  -- ^ The program memory
  , envBlockInfo :: RR.BlockInfo arch
  -- ^ Information on recovered basic blocks
  , envISA :: ISA.ISA arch
  -- ^ ISA for arch
  , envABI :: ABI.ABI arch
  -- ^ ABI for arch
  , envHandleAllocator :: C.HandleAllocator
  -- ^ Crucible handle allocator in use
  , envLogger :: LJ.LogAction IO (RCD.Diagnostic lm)
  -- ^ A logger used to stream diagnostics
  }

-- | A map of block addresses to the set of CFGs that contains them
-- (if any).
type BlockCFGIndex arch = M.Map (A.ConcreteAddress arch) (S.Set (FR.FunctionCFG arch))

-- | A monadic environment for binary rewriting
newtype RewriteM lm arch a = RewriteM { unRewriteM :: RWS.RWST (RewriteEnv lm arch) () (RewriteInfo arch) IO a }
  deriving (Applicative,
            Functor,
            Monad,
            RWS.MonadReader (RewriteEnv lm arch),
            RWS.MonadState (RewriteInfo arch),
            X.MonadThrow,
            RWS.MonadIO)

-- | Get all of the functions that the caller has injected into the binary
--
-- We need this accessor so that the redirection engine has the extra blocks
-- available to put them into the final block layout.
--
-- NOTE: This is mostly not user-facing, but it is used internally.  Users can
-- maintain the mapping if they really want it
getInjectedFunctions :: RewriteM lm arch [(A.SymbolicAddress arch, BS.ByteString)]
getInjectedFunctions = do
  m <- RWS.gets injectedFunctions
  return [ (a, bs) | (a, (_, bs)) <- M.toList m ]

getInjectedInstructions :: RewriteM lm arch [(A.SymbolicAddress arch, InjectSymbolicInstructions arch)]
getInjectedInstructions = do
  m <- RWS.gets injectedInstructions
  return [ (a, bs) | (a, (_, bs)) <- M.toList m ]

-- | Make a mapping from block addresses to their CFGs.
mkBlockCFGIndex :: [FR.FunctionCFG arch] -> BlockCFGIndex arch
mkBlockCFGIndex cfgs = F.foldl' indexCFGBlocks M.empty cfgs
  where
    indexCFGBlocks m c = F.foldl' (indexCFGBlock c) m (FR.cfgBlocks c)
    indexCFGBlock c m b = M.insertWith S.union b (S.singleton c) m

-- | Make a rewriter environment.
--
-- Used with 'runRewriteM', and also in analysis.
mkRewriteEnv
  :: LJ.LogAction IO (RCD.Diagnostic lm)
  -> [FR.FunctionCFG arch]
  -- ^ The control flow graphs discovered by previous analysis
  -> A.ConcreteAddress arch
  -> MM.Memory (MM.ArchAddrWidth arch)
  -> RR.BlockInfo arch
  -> ISA.ISA arch
  -> ABI.ABI arch
  -> C.HandleAllocator
  -> RewriteEnv lm arch
mkRewriteEnv logAction cfgs entryAddr mem blockInfo isa abi hAlloc = RewriteEnv
  { envBlockCFGIndex = mkBlockCFGIndex cfgs
  , envEntryAddress = entryAddr
  , envMemory = mem
  , envBlockInfo = blockInfo
  , envISA = isa
  , envABI = abi
  , envHandleAllocator = hAlloc
  , envLogger = logAction
  }

runRewriteM
  :: RewriteEnv lm arch
  -> A.ConcreteAddress arch
  -- ^ The address to start allocating new global variables at
  -> RS.SymbolicAddressAllocator arch
  -- ^ A symbolic address allocator for injected functions
  -> RewriteM lm arch a
  -- ^ The rewriting action to run
  -> IO (a, RewriteInfo arch)
runRewriteM env newGlobalBase symAlloc i = do
  (res, st, _) <- RWS.runRWST (unRewriteM i) env emptyInfo
  return (res, st)
  where
    emptyInfo = RewriteInfo { newGlobals = M.empty
                            , nextGlobalAddress = newGlobalBase
                            , symAddrAlloc = symAlloc
                            , injectedFunctions = M.empty
                            , injectedInstructions = M.empty
                            }

-- | Allow the caller to inject new code into the binary
--
-- The intent is that this is called in the rewriter setup phase, where the
-- caller can inject code and learn the symbolic address assigned to each block.
-- They can then store that information in the caller-specified info type and
-- pass the mapping (presumably from @'String' -> 'A.SymbolicAddress' arch@) to
-- the actual rewriting pass.
injectFunction :: String -> BS.ByteString -> RewriteM lm arch (A.SymbolicAddress arch)
injectFunction funcName bytes = do
  alloc0 <- RWS.gets symAddrAlloc
  let (addr, alloc1) = RS.nextSymbolicAddress alloc0
  RWS.modify' $ \s -> s { symAddrAlloc = alloc1
                        , injectedFunctions = M.insert addr (funcName, bytes) (injectedFunctions s)
                        }
  return addr

data InjectSymbolicInstructions arch where
  InjectSymbolicInstructions :: (RCI.InstructionConstraints arch tp)
                             => RCI.InstructionArchRepr arch tp
                             -> DLN.NonEmpty (RCI.Instruction arch tp (RCR.Relocation arch))
                             -> InjectSymbolicInstructions arch

injectInstructions
  :: (RCI.InstructionConstraints arch tp)
  => String
  -> RCI.InstructionArchRepr arch tp
  -> DLN.NonEmpty (RCI.Instruction arch tp (RCR.Relocation arch))
  -> RewriteM lm arch (A.SymbolicAddress arch)
injectInstructions funcName repr insns = do
  alloc0 <- RWS.gets symAddrAlloc
  let (addr, alloc1) = RS.nextSymbolicAddress alloc0
  let inj = InjectSymbolicInstructions repr insns
  RWS.modify' $ \s -> s { symAddrAlloc = alloc1
                        , injectedInstructions = M.insert addr (funcName, inj) (injectedInstructions s)
                        }
  return addr

-- | A function for instrumentors to call when they add
-- instrumentation to a binary.
recordRewrite :: (MM.MemWidth (MM.ArchAddrWidth arch)) => String -> B.SymbolicInfo arch -> Word -> RewriteM lm arch ()
recordRewrite ty baddr off = do
  logAction <- RWS.asks envLogger
  RWS.liftIO $ LJ.writeLog logAction (RCD.RewriteSite baddr off ty)

-- | Log a msg.
recordLogMsg :: lm -> RewriteM lm arch ()
recordLogMsg msg = do
  logAction <- RWS.asks envLogger
  RWS.liftIO $ LJ.writeLog logAction (RCD.Extension msg)

logDiagnostic :: RCD.Diagnostic lm -> RewriteM lm arch ()
logDiagnostic d = do
  logAction <- RWS.asks envLogger
  RWS.liftIO $ LJ.writeLog logAction d

-- | Get the ABI from environment.
getABI :: RewriteM lm arch (ABI.ABI arch)
getABI = RWS.asks envABI

-- | Get the ISA from environment.
getISA :: RewriteM lm arch (ISA.ISA arch)
getISA = RWS.asks envISA

getBlockIndex :: RewriteM lm arch (BlockCFGIndex arch)
getBlockIndex = RWS.asks envBlockCFGIndex


-- | Look up the unique CFG for the function containing the given 'SymbolicBlock', if any.
--
-- The block might not be assigned to a CFG, or assigned to multiple
-- CFGs, in which cases the function returns 'Nothing'.
lookupBlockCFG :: B.SymbolicBlock arch -> RewriteM lm arch (Maybe (FR.FunctionCFG arch))
lookupBlockCFG sb = do
  idx <- RWS.asks envBlockCFGIndex
  let mcfgs = M.lookup (B.symbolicBlockOriginalAddress sb) idx
  return $ do -- 'Maybe' monad
    [cfg] <- S.elems <$> mcfgs
    Just cfg

-- | Get the address of the entry point of the program
lookupEntryAddress :: RewriteM lm arch (A.ConcreteAddress arch)
lookupEntryAddress = RWS.asks envEntryAddress

-- | Allocate space for a global variable (occupying the given number
-- of bytes) in a new data section (one new data section for the
-- entire instrumentor run).
--
-- FIXME: The caller of runInstrument has to allocate a new data
-- segment for globals allocated here.
newGlobalVar :: (MM.MemWidth (MM.ArchAddrWidth arch)) => String -> Word32 -> RewriteM lm arch (A.ConcreteAddress arch)
newGlobalVar name size = do
  addr <- RWS.gets nextGlobalAddress
  RWS.modify' $ \s -> s { nextGlobalAddress = addr `A.addressAddOffset` fromIntegral size
                        , newGlobals = M.insert name addr (newGlobals s)
                        }
  return addr

isRelocatableTerminatorType :: Some (ISA.JumpType arch) -> Bool
isRelocatableTerminatorType jt =
  case jt of
    Some (ISA.IndirectJump {}) -> False
    Some (ISA.NotInstrumentable {}) -> False
    _ -> True

instrumentBlocks
  :: (MM.MemWidth (MM.ArchAddrWidth arch))
  => ISA.ISA arch
  -> RR.BlockInfo arch
  -> (A.ConcreteAddress arch, A.ConcreteAddress arch) -- ^ Text section extent
  -> (B.SymbolicBlock arch -> RewriteM lm arch (Maybe (B.ModifiedInstructions arch)))
  -> MM.Memory (MM.ArchAddrWidth arch)
  -> [(B.ConcreteBlock arch, B.SymbolicBlock arch)]
  -> RewriteM lm arch [RCL.WithProvenance B.SymbolicBlock arch]
instrumentBlocks isa blockInfo (textStart, textEnd) instrumentor mem baseSymBlocks =
  T.forM baseSymBlocks $ \(cb, sb) -> do    -- We only want to instrument blocks that:
    --
    -- 1. Live in the .text
    -- 2. Do not rely on their location (e.g. via PIC jumps)
    -- 3. Do not reside in incomplete functions (where unknown control flow might break our assumptions)
    -- 4. Do not overlap other blocks (which are hard for us to rewrite)
    --
    -- Also, see Note [PIC Jump Tables]
    case and [ textStart <= B.concreteBlockAddress cb
             , B.concreteBlockAddress cb < textEnd
             , isRelocatableTerminatorType (B.terminatorType isa mem cb)
             , not (RR.isIncompleteBlockAddress blockInfo (B.concreteBlockAddress cb))
             , disjoint isa (RR.biOverlap blockInfo) cb
             ] of
     True ->  do
       maybeMod <- instrumentor sb
       case maybeMod of
         Just (B.ModifiedInstructions repr' insns') -> do
           let sb' = B.symbolicBlock (B.symbolicBlockOriginalAddress sb)
                                     (B.symbolicBlockSymbolicAddress sb)
                                     insns'
                                     repr'
                                     (B.symbolicBlockSymbolicSuccessor sb)
           return $! RCL.WithProvenance cb sb' RCL.Modified
         Nothing      ->
           return $! RCL.WithProvenance cb sb RCL.Unmodified
     False -> do
       let term = B.terminatorType isa mem cb
       let isIncomp = RR.isIncompleteBlockAddress blockInfo (B.concreteBlockAddress cb)
       let isDisjoint = disjoint isa (RR.biOverlap blockInfo) cb
       logDiagnostic (RCD.RedirectionDiagnostic (RCD.CannotRedirect isa cb term isIncomp isDisjoint))

       return $! RCL.WithProvenance cb sb RCL.Immutable
