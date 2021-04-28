{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Renovate.Rewrite (
  RewriteMT,
  RewriteM,
  RewriteInfo(..),
  RewriteSite(..),
  BlockCFGIndex,
  mkRewriteEnv,
  runRewriteMT,
  runRewriteM,
  hoist,
  lookupBlockCFG,
  lookupEntryAddress,
  HasInjectedFunctions(..),
  newGlobalVar,
  getBlockIndex,
  getABI,
  getISA,
  InjectSymbolicInstructions(..),
  injectInstructions,
  injectFunction,
  recordRewrite,
  recordLogMsg,
  ) where

import qualified Control.Monad.Identity as I
import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Word ( Word32 )

import qualified Data.Macaw.CFG as MM

import qualified Lang.Crucible.FunctionHandle as C

import qualified Renovate.Address as A
import qualified Renovate.Analysis.FunctionRecovery as FR
import qualified Renovate.BasicBlock as B
import qualified Renovate.ABI as ABI
import qualified Renovate.ISA as ISA
import qualified Renovate.Recovery as RR
import qualified Renovate.Redirect.Symbolize as RS

data RewriteSite arch =
  RewriteSite { siteDescriptor :: (B.SymbolicInfo arch, Word)
              -- ^ The basic block modified and the instruction offset into that
              -- block.
              , siteType :: String
              -- ^ A human-readable description of the modification
              }
  deriving (Eq, Ord)

deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (RewriteSite arch)

-- | The state for the 'RewriteM' monad.
--
-- The @lm@ type is an arbitrary user controlled type that can be
-- logged during rewriting using 'recordLogMsg'.
data RewriteInfo lm arch =
  RewriteInfo { infoSites :: [RewriteSite arch]
              -- ^ A collection of all of the rewritings applied so far
              , newGlobals :: M.Map String (A.ConcreteAddress arch)
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
              , logMsgs :: [lm]
              -- ^ A collection of msgs logged during rewriting.
              }

class HasInjectedFunctions m arch where
  -- | Get all of the functions that the caller has injected into the binary
  --
  -- We need this accessor so that the redirection engine has the extra blocks
  -- available to put them into the final block layout.
  --
  -- NOTE: This is mostly not user-facing, but it is used internally.  Users can
  -- maintain the mapping if they really want it
  getInjectedFunctions :: m [(A.SymbolicAddress arch, BS.ByteString)]
  getInjectedInstructions :: m [(A.SymbolicAddress arch, InjectSymbolicInstructions arch)]

data RewriteEnv arch = RewriteEnv
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
  }
-- | A map of block addresses to the set of CFGs that contains them
-- (if any).
type BlockCFGIndex arch = M.Map (A.ConcreteAddress arch) (S.Set (FR.FunctionCFG arch))

-- | A monadic environment for binary rewriting
newtype RewriteMT lm arch m a = RewriteM { unRewriteM :: RWS.RWST (RewriteEnv arch) () (RewriteInfo lm arch) m a }
  deriving (Applicative,
            Functor,
            Monad,
            RWS.MonadReader (RewriteEnv arch),
            RWS.MonadState (RewriteInfo lm arch),
            RWS.MonadIO)

type RewriteM lm arch = RewriteMT lm arch I.Identity

instance Monad m => HasInjectedFunctions (RewriteMT lm arch m) arch where
  getInjectedFunctions = getInj
  getInjectedInstructions = getInjInsns

getInj :: Monad m => RewriteMT lm arch m [(A.SymbolicAddress arch, BS.ByteString)]
getInj = do
  m <- RWS.gets injectedFunctions
  return [ (a, bs) | (a, (_, bs)) <- M.toList m ]

getInjInsns :: (Monad m) => RewriteMT lm arch m [(A.SymbolicAddress arch, InjectSymbolicInstructions arch)]
getInjInsns = do
  m <- RWS.gets injectedInstructions
  return [ (a, bs) | (a, (_, bs)) <- M.toList m ]

hoist :: Monad m => RewriteM lm arch a -> RewriteMT lm arch m a
hoist (RewriteM act) = RewriteM (RWS.mapRWST (return . I.runIdentity) act)

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
  :: [FR.FunctionCFG arch]
  -- ^ The control flow graphs discovered by previous analysis
  -> A.ConcreteAddress arch
  -> MM.Memory (MM.ArchAddrWidth arch)
  -> RR.BlockInfo arch
  -> ISA.ISA arch
  -> ABI.ABI arch
  -> C.HandleAllocator
  -> RewriteEnv arch
mkRewriteEnv cfgs entryAddr mem blockInfo isa abi hAlloc = RewriteEnv
  { envBlockCFGIndex = mkBlockCFGIndex cfgs
  , envEntryAddress = entryAddr
  , envMemory = mem
  , envBlockInfo = blockInfo
  , envISA = isa
  , envABI = abi
  , envHandleAllocator = hAlloc
  }

-- | Run rewriting computation and return its value, along with metadata about
-- transformations applied.
runRewriteM :: RewriteEnv arch
            -> A.ConcreteAddress arch
            -- ^ The address to start allocating new global variables at
            -> RS.SymbolicAddressAllocator arch
            -- ^ A symbolic address allocator for injected functions
            -> RewriteM lm arch a
            -- ^ The rewriting action to run
            -> (a, RewriteInfo lm arch)
runRewriteM env newGlobalBase symAlloc = I.runIdentity . runRewriteMT env newGlobalBase symAlloc

runRewriteMT :: Monad m
             => RewriteEnv arch
             -> A.ConcreteAddress arch
             -- ^ The address to start allocating new global variables at
             -> RS.SymbolicAddressAllocator arch
             -- ^ A symbolic address allocator for injected functions
             -> RewriteMT lm arch m a
             -- ^ The rewriting action to run
             -> m (a, RewriteInfo lm arch)
runRewriteMT env newGlobalBase symAlloc i = do
  (res, st, _) <- RWS.runRWST (unRewriteM i) env emptyInfo
  return (res, st)
  where
    emptyInfo = RewriteInfo { infoSites = []
                            , newGlobals = M.empty
                            , nextGlobalAddress = newGlobalBase
                            , symAddrAlloc = symAlloc
                            , injectedFunctions = M.empty
                            , injectedInstructions = M.empty
                            , logMsgs = []
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
  InjectSymbolicInstructions :: (B.ArchConstraints arch tp)
                             => B.InstructionArchRepr arch tp
                             -> DLN.NonEmpty (B.Instruction arch tp (B.Relocation arch))
                             -> InjectSymbolicInstructions arch

injectInstructions
  :: (B.ArchConstraints arch tp)
  => String
  -> B.InstructionArchRepr arch tp
  -> DLN.NonEmpty (B.Instruction arch tp (B.Relocation arch))
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
recordRewrite :: String -> B.SymbolicInfo arch -> Word -> RewriteM lm arch ()
recordRewrite ty baddr off =
  RWS.modify $ \s -> s { infoSites = site : infoSites s }
  where
    site = RewriteSite { siteDescriptor = (baddr, off)
                       , siteType = ty
                       }

-- | Log a msg.
recordLogMsg :: lm -> RewriteM lm arch ()
recordLogMsg msg =
  RWS.modify $ \s -> s { logMsgs = msg : logMsgs s }

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
