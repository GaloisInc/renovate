{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
-- | This module defines a 'Monad' for the binary rewriter.
--
-- The monad basically provides facilities to allocate symbolic
-- addresses for code relocation, and also provides for error
-- handling.
module Renovate.Redirect.Monad (
  SomeAddr(..),
  Diagnostic(..),
  Rewriter,
  RewriterT,
  SymbolMap,
  NewSymbolsMap,
  RewriterState(..),
  RewriterStats(..),
  Diagnostics,
  RewriterResult(..),
  runRewriterT,
  throwError,
  logDiagnostic,
  askISA,
  askMem,
  askSymbolMap,
  putNewSymbolsMap,
  getNewSymbolsMap,
  emptyRewriterStats,
  recordUnrelocatableTermBlock,
  recordIncompleteBlock,
  recordUnrelocatableSize,
  recordReusedBytes,
  recordDiscoveredBlock,
  recordInstrumentedBytes,
  recordBlockMap,
  recordFunctionBlocks,
  ) where


import qualified Control.Monad.Catch as E
import qualified Control.Monad.Except as ET
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.Trans as T
import qualified Data.Functor.Identity as I
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import qualified Data.Sequence as Seq
import           GHC.Generics ( Generic )

import           Prelude

import qualified Data.Macaw.CFG as MM

import           Renovate.Address
import           Renovate.ISA
import           Renovate.Diagnostic
import           Renovate.Rewrite ( HasInjectedFunctions )
import           Renovate.Recovery.SymbolMap ( SymbolMap, NewSymbolsMap )

data SomeAddr a = Addr32 (a 32)
                | Addr64 (a 64)

deriving instance (Eq (a 32), Eq (a 64)) => Eq (SomeAddr a)
deriving instance (Ord (a 32), Ord (a 64)) => Ord (SomeAddr a)
deriving instance (Show (a 32), Show (a 64)) => Show (SomeAddr a)

-- | Reader data for 'RewriterT'.
data RewriterEnv arch = RewriterEnv
  { reISA       :: !(ISA arch)
  , reMem       :: !(MM.Memory (MM.ArchAddrWidth arch))
  , reSymbolMap :: !(SymbolMap arch)
  }

-- | State data for 'RewriterT'.
data RewriterState arch = RewriterState
  { rwsNewSymbolsMap         :: !(NewSymbolsMap arch)
  , rwsStats                 :: !(RewriterStats arch)
  }
deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (RewriterState arch)

-- | Some statistics about a rewrite that it might be interesting to report to the user.
data RewriterStats arch = RewriterStats
  { unrelocatableTerm     :: !Int
  -- ^ Count of blocks unrelocatable due to ending in an IP-relative indirect jump
  , smallBlockCount       :: !Int
  -- ^ Count of blocks unrelocatable due to being too small to redirect
  , reusedByteCount       :: !Int
  -- ^ Count of bytes re-used by the compact layout strategy
  , incompleteBlocks      :: !Int
  -- ^ Count of blocks that are in incomplete functions
  , discoveredBlocks      :: !(Map (ConcreteAddress arch) Int)
  -- ^ The addresses and byte counts of each block discovered
  , instrumentedBytes     :: !Int
  -- ^ A count of the bytes in blocks that were modified by the instrumentor
  , blockMapping          :: [(ConcreteAddress arch, ConcreteAddress arch)]
  -- ^ A mapping of original block addresses to the address they were redirected to
  , functionBlocks        :: !(Map (ConcreteAddress arch) [ConcreteAddress arch])
  -- ^ Keys are the addresses of function entry points; values are the addresses of all blocks contained in that function
  } deriving (Generic)
deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (RewriterStats arch)

emptyRewriterStats :: RewriterStats arch
emptyRewriterStats = RewriterStats
  { unrelocatableTerm = 0
  , smallBlockCount   = 0
  , reusedByteCount   = 0
  , incompleteBlocks  = 0
  , discoveredBlocks  = M.empty
  , instrumentedBytes = 0
  , blockMapping      = []
  , functionBlocks    = M.empty
  }

-- | Result data of 'RewriterT', the combination of state and writer results.
data RewriterResult arch = RewriterResult
  { rrState :: RewriterState arch
  , rrDiagnostics :: Diagnostics
  }

-- | The base 'Monad' for the binary rewriter and relocation code.
--
-- It provides a source for symbolic addresses and provides error
-- handling facilities.
newtype RewriterT arch m a =
  RewriterT { unRewriterT :: ET.ExceptT E.SomeException
                                        (RWS.RWST (RewriterEnv arch)
                                                  Diagnostics
                                                  (RewriterState arch)
                                                  m)
                                         a
            }
  deriving (Applicative,
            Functor,
            Monad,
            RWS.MonadReader (RewriterEnv arch),
            RWS.MonadState  (RewriterState arch),
            RWS.MonadWriter Diagnostics,
            ET.MonadError E.SomeException)

instance T.MonadIO m => T.MonadIO (RewriterT arch m) where
  liftIO = RewriterT . T.liftIO

-- | A 'RewriterT' over the 'I.Identity' 'Monad'
type Rewriter arch a = RewriterT arch I.Identity a

instance T.MonadTrans (RewriterT arch) where
  lift m = RewriterT $ ET.ExceptT $ do
    res <- RWS.RWST $ \_ s -> do
      a <- m
      return (a, s, mempty)
    return (Right res)

-- | The initial state of the 'Rewriter' 'Monad'
initialState :: RewriterState arch
initialState =  RewriterState
  { rwsNewSymbolsMap         = mempty
  , rwsStats                 = emptyRewriterStats
  }

-- | Run a 'RewriterT' computation.
--
-- It returns *all* diagnostics that occur before an exception is
-- thrown.
--
-- FIXME: This needs the set of input additional blocks that are allocated symbolic addresses
runRewriterT :: (Monad m, HasInjectedFunctions m arch)
             => ISA arch
             -> MM.Memory (MM.ArchAddrWidth arch)
             -> SymbolMap arch
             -> RewriterT arch m a
             -> m (Either E.SomeException a, RewriterResult arch)
runRewriterT isa mem symmap a = do
  let env = RewriterEnv isa mem symmap
  (r, s, w) <- RWS.runRWST (ET.runExceptT (unRewriterT a)) env initialState
  return (r, RewriterResult s w)

-- | Log a diagnostic in the 'RewriterT' monad
logDiagnostic :: (Monad m) => Diagnostic -> RewriterT arch m ()
logDiagnostic = RWS.tell . Diagnostics . Seq.singleton

-- | Throw an error that halts the 'RewriterT' monad.
throwError :: (E.Exception e, Monad m) => e -> RewriterT arch m a
throwError = ET.throwError . E.SomeException

-- | Read the 'ISA' from the 'RewriterT' environment
askISA :: (Monad m) => RewriterT arch m (ISA arch)
askISA = reISA <$> RWS.ask

askMem :: (Monad m) => RewriterT arch m (MM.Memory (MM.ArchAddrWidth arch))
askMem = reMem <$> RWS.ask

askSymbolMap :: Monad m => RewriterT arch m (SymbolMap arch)
askSymbolMap = reSymbolMap <$> RWS.ask

putNewSymbolsMap :: Monad m => NewSymbolsMap arch -> RewriterT arch m ()
putNewSymbolsMap symmap = do
  s <- RWS.get
  RWS.put $! s { rwsNewSymbolsMap = symmap }

getNewSymbolsMap :: Monad m => RewriterT arch m (NewSymbolsMap arch)
getNewSymbolsMap = do
  s <- RWS.get
  return $! rwsNewSymbolsMap s

onStats :: Monad m => (RewriterStats arch -> RewriterStats arch) -> RewriterT arch m ()
onStats f = RWS.modify $! \s -> s { rwsStats = f (rwsStats s) }

recordUnrelocatableTermBlock :: (Monad m) => RewriterT arch m ()
recordUnrelocatableTermBlock = onStats $ \s -> s { unrelocatableTerm = unrelocatableTerm s + 1 }

recordUnrelocatableSize :: (Monad m) => RewriterT arch m ()
recordUnrelocatableSize = onStats $ \s -> s { smallBlockCount = smallBlockCount s + 1 }

recordReusedBytes :: (Monad m) => Int -> RewriterT arch m ()
recordReusedBytes nBytes = onStats $ \s -> s { reusedByteCount = reusedByteCount s + nBytes }

recordBlockMap :: (Monad m) => [(ConcreteAddress arch, ConcreteAddress arch)] -> RewriterT arch m ()
recordBlockMap m = onStats $ \s -> s { blockMapping = m }

recordIncompleteBlock :: (Monad m) => RewriterT arch m ()
recordIncompleteBlock = onStats $ \s -> s { incompleteBlocks = incompleteBlocks s + 1 }

recordDiscoveredBlock :: Monad m => ConcreteAddress arch -> Int -> RewriterT arch m ()
recordDiscoveredBlock addr nBytes = onStats $ \s -> s { discoveredBlocks = M.insert addr nBytes (discoveredBlocks s) }

recordInstrumentedBytes :: Monad m => Int -> RewriterT arch m ()
recordInstrumentedBytes nBytes = onStats $ \s -> s { instrumentedBytes = instrumentedBytes s + nBytes }

recordFunctionBlocks :: Monad m => Map (ConcreteAddress arch) [ConcreteAddress arch] -> RewriterT arch m ()
recordFunctionBlocks m = onStats $ \s -> s { functionBlocks = m }
