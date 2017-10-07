{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Renovate.Rewrite (
  RewriteM,
  RewriteInfo(..),
  RewriteSite(..),
  runRewriteM,
  lookupBlockCFG,
  lookupEntryAddress,
  newGlobalVar,
  lookupGlobalVar,
  recordRewrite
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import           Data.Word ( Word32 )

import qualified Data.Macaw.Memory as MM

import qualified Renovate.Address as A
import qualified Renovate.BasicBlock as B
import qualified Renovate.Analysis.FunctionRecovery as FR

data RewriteSite w =
  RewriteSite { siteDescriptor :: (B.SymbolicInfo w, Word)
              -- ^ The basic block modified and the instruction offset into that
              -- block.
              , siteType :: String
              -- ^ A human-readable description of the modification
              }
  deriving (Eq, Ord, Show)

data RewriteInfo w =
  RewriteInfo { infoSites :: [RewriteSite w]
              -- ^ A collection of all of the rewritings applied so far
              , newGlobals :: M.Map String (A.RelAddress w)
              -- ^ A mapping of names to globals allocated in a new data
              -- section.  The names are user-provided and must be unique.
              , nextGlobalAddress :: A.RelAddress w
              -- ^ The next address that will be handed out for a new global
              -- variable.
              }

data RewriteEnv i w =
  RewriteEnv { envCFGs :: M.Map (A.RelAddress w) (FR.FunctionCFG i w)
             -- ^ A map of function entry point addresses to CFGs
             , envBlockCFGIndex :: M.Map (A.RelAddress w) (FR.FunctionCFG i w)
             -- ^ A map of block addresses to the CFG that contains them (if
             -- any)
             , envEntryAddress :: A.RelAddress w
             , envMemory :: MM.Memory w
             }

-- | A monadic environment for binary rewriting
newtype RewriteM i w a = RewriteM { unRewriteM :: RWS.RWS (RewriteEnv i w) () (RewriteInfo w) a }
  deriving (Applicative,
            Functor,
            Monad,
            RWS.MonadReader (RewriteEnv i w),
            RWS.MonadState (RewriteInfo w))

-- | Run rewriting computation and return its value, along with metadata about
-- transformations applied.
runRewriteM :: MM.Memory w
            -> A.RelAddress w
            -- ^ The address of the entry point of the program
            -> A.RelAddress w
            -- ^ The address to start allocating new global variables at
            -> [FR.FunctionCFG i w]
            -- ^ The control flow graphs discovered by previous analysis
            -> RewriteM i w a
            -- ^ The rewriting action to run
            -> (a, RewriteInfo w)
runRewriteM mem entryAddr newGlobalBase cfgs i = (res, st)
  where
    (res, st, _) = RWS.runRWS (unRewriteM i) env emptyInfo
    emptyInfo = RewriteInfo { infoSites = []
                            , newGlobals = M.empty
                            , nextGlobalAddress = newGlobalBase
                            }
    env = RewriteEnv { envCFGs = F.foldl' addCFG M.empty cfgs
                     , envBlockCFGIndex = F.foldl' indexCFGBlocks M.empty cfgs
                     , envEntryAddress = entryAddr
                     , envMemory = mem
                     }
    addCFG m c = M.insert (FR.cfgEntry c) c m
    indexCFGBlocks m c = F.foldl' (indexCFGBlock c) m (FR.cfgBlocks c)
    indexCFGBlock c m b = M.insert (B.basicBlockAddress b) c m

-- | A function for instrumentors to call when they add
-- instrumentation to a binary.
recordRewrite :: String -> B.SymbolicInfo w -> Word -> RewriteM i w ()
recordRewrite ty baddr off =
  RWS.modify $ \s -> s { infoSites = site : infoSites s }
  where
    site = RewriteSite { siteDescriptor = (baddr, off)
                       , siteType = ty
                       }

-- | Look up the CFG for the function containing the given 'SymbolicBlock', if any.
--
-- The block might not be assigned to a CFG, in which case the
-- function returns 'Nothing'
lookupBlockCFG :: B.SymbolicBlock i a w -> RewriteM i w (Maybe (FR.FunctionCFG i w))
lookupBlockCFG sb = do
  idx <- RWS.asks envBlockCFGIndex
  return (M.lookup (B.concreteAddress (B.basicBlockAddress sb)) idx)

-- | Get the address of the entry point of the program
lookupEntryAddress :: RewriteM i w (A.RelAddress w)
lookupEntryAddress = RWS.asks envEntryAddress

-- | Allocate space for a global variable (occupying the given number
-- of bytes) in a new data section (one new data section for the
-- entire instrumentor run).
--
-- FIXME: The caller of runInstrument has to allocate a new data
-- segment for globals allocated here.
newGlobalVar :: (MM.MemWidth w) => String -> Word32 -> RewriteM i w (A.RelAddress w)
newGlobalVar name size = do
  addr <- RWS.gets nextGlobalAddress
  mem  <- RWS.asks envMemory
  let addOff = A.addressAddOffset mem
  RWS.modify' $ \s -> s { nextGlobalAddress = addr `addOff` fromIntegral size
                        , newGlobals = M.insert name addr (newGlobals s)
                        }
  return addr

-- | Look up a previously-defined global variable by its name
--
-- This will fail if the variable was not yet allocated, as that is a
-- programming error.
lookupGlobalVar :: String -> RewriteM i w (A.RelAddress w)
lookupGlobalVar name = do
  m <- RWS.gets newGlobals
  case M.lookup name m of
    Just a -> return a
    Nothing -> L.error ("Global variable [" ++ name ++ "] is not defined")
