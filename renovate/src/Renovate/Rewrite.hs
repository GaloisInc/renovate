{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Renovate.Rewrite (
  RewriteM,
  RewriteEnv(..),
  RewriteInfo(..),
  RewriteSite(..),
  BlockCFGIndex,
  mkRewriteEnv,
  runRewriteM,
  lookupBlockCFG,
  lookupEntryAddress,
  newGlobalVar,
  lookupGlobalVar,
  getISA,
  recordRewrite
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Word ( Word32 )

import qualified Data.Macaw.CFG as MM

import qualified Renovate.Address as A
import qualified Renovate.Analysis.FunctionRecovery as FR
import qualified Renovate.BasicBlock as B
import qualified Renovate.ISA as ISA
import qualified Renovate.Recovery as RR

data RewriteSite arch =
  RewriteSite { siteDescriptor :: (B.SymbolicInfo arch, Word)
              -- ^ The basic block modified and the instruction offset into that
              -- block.
              , siteType :: String
              -- ^ A human-readable description of the modification
              }
  deriving (Eq, Ord)

deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (RewriteSite arch)

data RewriteInfo arch =
  RewriteInfo { infoSites :: [RewriteSite arch]
              -- ^ A collection of all of the rewritings applied so far
              , newGlobals :: M.Map String (A.ConcreteAddress arch)
              -- ^ A mapping of names to globals allocated in a new data
              -- section.  The names are user-provided and must be unique.
              , nextGlobalAddress :: A.ConcreteAddress arch
              -- ^ The next address that will be handed out for a new global
              -- variable.
              }

data RewriteEnv arch =
  RewriteEnv { envBlockCFGIndex :: BlockCFGIndex arch
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
             }
-- | A map of block addresses to the set of CFGs that contains them
-- (if any).
type BlockCFGIndex arch = M.Map (A.ConcreteAddress arch) (S.Set (FR.FunctionCFG arch))

-- | A monadic environment for binary rewriting
newtype RewriteM arch a = RewriteM { unRewriteM :: RWS.RWS (RewriteEnv arch) () (RewriteInfo arch) a }
  deriving (Applicative,
            Functor,
            Monad,
            RWS.MonadReader (RewriteEnv arch),
            RWS.MonadState (RewriteInfo arch))

-- | Make a mapping from block addresses to their CFGs.
mkBlockCFGIndex :: [FR.FunctionCFG arch] -> BlockCFGIndex arch
mkBlockCFGIndex cfgs = F.foldl' indexCFGBlocks M.empty cfgs
  where
    indexCFGBlocks m c = F.foldl' (indexCFGBlock c) m (FR.cfgBlocks c)
    indexCFGBlock c m b = M.insertWith S.union b (S.singleton c) m

-- | Make a rewriter environment.
--
-- Used with 'runRewriteM', and also in analysis.
mkRewriteEnv :: [FR.FunctionCFG arch]
                -- ^ The control flow graphs discovered by previous analysis
             -> A.ConcreteAddress arch
             -> MM.Memory (MM.ArchAddrWidth arch)
             -> RR.BlockInfo arch
             -> ISA.ISA arch
             -> RewriteEnv arch
mkRewriteEnv cfgs entryAddr mem blockInfo isa =
  RewriteEnv { envBlockCFGIndex = mkBlockCFGIndex cfgs
             , envEntryAddress = entryAddr
             , envMemory = mem
             , envBlockInfo = blockInfo
             , envISA = isa
             }

-- | Run rewriting computation and return its value, along with metadata about
-- transformations applied.
runRewriteM :: RewriteEnv arch
            -> A.ConcreteAddress arch
            -- ^ The address to start allocating new global variables at
            -> RewriteM arch a
            -- ^ The rewriting action to run
            -> (a, RewriteInfo arch)
runRewriteM env newGlobalBase i = (res, st)
  where
    (res, st, _) = RWS.runRWS (unRewriteM i) env emptyInfo
    emptyInfo = RewriteInfo { infoSites = []
                            , newGlobals = M.empty
                            , nextGlobalAddress = newGlobalBase
                            }

-- | A function for instrumentors to call when they add
-- instrumentation to a binary.
recordRewrite :: String -> B.SymbolicInfo arch -> Word -> RewriteM arch ()
recordRewrite ty baddr off =
  RWS.modify $ \s -> s { infoSites = site : infoSites s }
  where
    site = RewriteSite { siteDescriptor = (baddr, off)
                       , siteType = ty
                       }

-- | Get the ISA from environment.
getISA :: RewriteM arch (ISA.ISA arch)
getISA = RWS.asks envISA

-- | Look up the unique CFG for the function containing the given 'SymbolicBlock', if any.
--
-- The block might not be assigned to a CFG, or assigned to multiple
-- CFGs, in which cases the function returns 'Nothing'.
lookupBlockCFG :: B.SymbolicBlock arch -> RewriteM arch (Maybe (FR.FunctionCFG arch))
lookupBlockCFG sb = do
  idx <- RWS.asks envBlockCFGIndex
  let mcfgs = M.lookup (B.concreteAddress (B.basicBlockAddress sb)) idx
  return $ do -- 'Maybe' monad
    [cfg] <- S.elems <$> mcfgs
    Just cfg

-- | Get the address of the entry point of the program
lookupEntryAddress :: RewriteM arch (A.ConcreteAddress arch)
lookupEntryAddress = RWS.asks envEntryAddress

-- | Allocate space for a global variable (occupying the given number
-- of bytes) in a new data section (one new data section for the
-- entire instrumentor run).
--
-- FIXME: The caller of runInstrument has to allocate a new data
-- segment for globals allocated here.
newGlobalVar :: (MM.MemWidth (MM.ArchAddrWidth arch)) => String -> Word32 -> RewriteM arch (A.ConcreteAddress arch)
newGlobalVar name size = do
  addr <- RWS.gets nextGlobalAddress
  RWS.modify' $ \s -> s { nextGlobalAddress = addr `A.addressAddOffset` fromIntegral size
                        , newGlobals = M.insert name addr (newGlobals s)
                        }
  return addr

-- | Look up a previously-defined global variable by its name
--
-- This will fail if the variable was not yet allocated, as that is a
-- programming error.
lookupGlobalVar :: String -> RewriteM arch (A.ConcreteAddress arch)
lookupGlobalVar name = do
  m <- RWS.gets newGlobals
  case M.lookup name m of
    Just a -> return a
    Nothing -> L.error ("Global variable [" ++ name ++ "] is not defined")
