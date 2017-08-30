{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A monad that all instrumentors run in
--
-- Its primary purpose is to allow transformations to record the
-- changes they make to programs.
module Renovate.Instrument.Monad (
  Instrument,
  InstrumentInfo(..),
  InstrumentationSite(..),
  runInstrument,
  lookupBlockCFG,
  lookupEntryAddress,
  newGlobalVar,
  lookupGlobalVar,
  recordInstrumentation
  ) where

import qualified GHC.Err.Located as L

import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import           Data.Word ( Word32 )

import qualified Data.Macaw.Memory as MM

import           Renovate.Address
import           Renovate.BasicBlock
import           Renovate.Analysis.FunctionRecovery

-- | A descriptor for an instrumentation site.  It includes
-- information about the block containing the instrumentation, as well
-- as a string description.
--
-- It would be nice to have more structure than a 'String'
data InstrumentationSite w =
  InstrumentationSite { siteDescriptor :: (SymbolicInfo w, Word)
                      -- ^ The basic block modified and the
                      -- instruction offset into that block.  this
                      -- might not be very stable
                      , instrumentationType :: String
                      }
  deriving (Eq, Ord, Show)

-- | Information generated during an instrumentation run.  It
-- currently tracks all of the locations instrumentation was inserted
-- at.
--
-- It also tracks the space required to be reserved in a newly
-- allocated data section.  Right now, the extra space is going to be
-- all zero-initialized.
data InstrumentInfo w =
  InstrumentInfo { infoSites :: [InstrumentationSite w]
                 , newGlobals :: M.Map String (RelAddress w)
                 , nextGlobalAddress :: RelAddress w
                 }

data InstrumentEnv i w =
  InstrumentEnv { envCFGs :: M.Map (RelAddress w) (FunctionCFG i w)
                -- ^ A map of function entry point addresses to CFGs
                , envBlockCFGIndex :: M.Map (RelAddress w) (FunctionCFG i w)
                -- ^ A map of block addresses to their corresponding
                -- CFG (if any)
                , envEntryAddress :: RelAddress w
                -- ^ The address of the entry block
                }

-- | A 'Monad' for binary instrumentors
newtype Instrument i w a = Instrument { unInstrument :: RWS.RWS (InstrumentEnv i w) () (InstrumentInfo w) a }
  deriving (Applicative,
            Functor,
            Monad,
            RWS.MonadReader (InstrumentEnv i w),
            RWS.MonadState (InstrumentInfo w))

-- | Run an 'Instrument'ation computation anad return its value, along
-- with metadata about instrumentation sites.
runInstrument :: RelAddress w -> RelAddress w -> [FunctionCFG i w] -> Instrument i w a -> (a, InstrumentInfo w)
runInstrument entryAddr newGlobalBase cfgs i = (res, st)
  where
    (res, st, _) = RWS.runRWS (unInstrument i) env emptyInfo
    emptyInfo = InstrumentInfo { infoSites = []
                               , newGlobals = M.empty
                               , nextGlobalAddress = newGlobalBase
                               }
    env = InstrumentEnv { envCFGs = F.foldl' addCFG M.empty cfgs
                        , envBlockCFGIndex = F.foldl' indexCFGBlocks M.empty cfgs
                        , envEntryAddress = entryAddr
                        }
    addCFG m c = M.insert (cfgEntry c) c m
    indexCFGBlocks m c = F.foldl' (indexCFGBlock c) m (cfgBlocks c)
    indexCFGBlock c m b = M.insert (basicBlockAddress b) c m

-- | A function for instrumentors to call when they add
-- instrumentation to a binary.
recordInstrumentation :: String -> SymbolicInfo w -> Word -> Instrument i w ()
recordInstrumentation ty baddr off =
  RWS.modify $ \s -> s { infoSites = site : infoSites s }
  where
    site = InstrumentationSite { siteDescriptor = (baddr, off)
                               , instrumentationType = ty
                               }

-- | Look up the CFG for the function containing the given 'SymbolicBlock', if any.
--
-- The block might not be assigned to a CFG, in which case the
-- function returns 'Nothing'
lookupBlockCFG :: SymbolicBlock i a w -> Instrument i w (Maybe (FunctionCFG i w))
lookupBlockCFG sb = do
  idx <- RWS.asks envBlockCFGIndex
  return (M.lookup (concreteAddress (basicBlockAddress sb)) idx)

-- | Get the address of the entry point of the program
lookupEntryAddress :: Instrument i w (RelAddress w)
lookupEntryAddress = RWS.asks envEntryAddress

-- | Allocate space for a global variable (occupying the given number
-- of bytes) in a new data section (one new data section for the
-- entire instrumentor run).
--
-- FIXME: The caller of runInstrument has to allocate a new data
-- segment for globals allocated here.
newGlobalVar :: (MM.MemWidth w) => String -> Word32 -> Instrument i w (RelAddress w)
newGlobalVar name size = do
  addr <- RWS.gets nextGlobalAddress
  RWS.modify' $ \s -> s { nextGlobalAddress = addr `addressAddOffset` fromIntegral size
                        , newGlobals = M.insert name addr (newGlobals s)
                        }
  return addr

-- | Look up a previously-defined global variable by its name
--
-- This will fail if the variable was not yet allocated, as that is a
-- programming error.
lookupGlobalVar :: String -> Instrument i w (RelAddress w)
lookupGlobalVar name = do
  m <- RWS.gets newGlobals
  case M.lookup name m of
    Just a -> return a
    Nothing -> L.error ("Global variable [" ++ name ++ "] is not defined")
