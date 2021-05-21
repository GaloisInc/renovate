{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
  RedirectT,
  SymbolMap,
  NewSymbolsMap,
  RewriterState(..),
  SectionInfo(..),
  runRedirectT,
  logDiagnostic,
  askISA,
  askMem,
  askSymbolMap,
  putNewSymbolsMap,
  getNewSymbolsMap
  ) where

import qualified Control.Monad.Catch as X
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.Trans as T
import qualified Data.Map.Strict as Map
import           Data.Monoid
import qualified Lumberjack as LJ

import           Prelude

import qualified Data.Macaw.CFG as MM

import           Renovate.Core.Address
import qualified Renovate.Core.Diagnostic as RCD
import qualified Renovate.ISA as ISA
import           Renovate.Recovery.SymbolMap ( SymbolMap, NewSymbolsMap )

data SomeAddr a = Addr32 (a 32)
                | Addr64 (a 64)

deriving instance (Eq (a 32), Eq (a 64)) => Eq (SomeAddr a)
deriving instance (Ord (a 32), Ord (a 64)) => Ord (SomeAddr a)
deriving instance (Show (a 32), Show (a 64)) => Show (SomeAddr a)

-- | Reader data for 'RedirectT'.
data RewriterEnv lm arch = RewriterEnv
  { reISA       :: !(ISA.ISA arch)
  , reMem       :: !(MM.Memory (MM.ArchAddrWidth arch))
  , reSymbolMap :: !(SymbolMap arch)
  , reLogAction :: LJ.LogAction IO (RCD.Diagnostic lm)
  }

-- | State data for 'RedirectT'.
data RewriterState arch = RewriterState
  { rwsNewSymbolsMap         :: !(NewSymbolsMap arch)
  , rwsBlockMapping          :: [(ConcreteAddress arch, ConcreteAddress arch)]
  -- ^ A mapping of original block addresses to the address they were redirected to
  , rwsBackwardBlockMapping  :: !(Map.Map (ConcreteAddress arch) (ConcreteAddress arch))
  -- ^ A mapping of rewritten block addresses to the address from the original that should have the same behavior
  , rwsFunctionBlocks        :: !(Map.Map (ConcreteAddress arch) [ConcreteAddress arch])
  -- ^ Keys are the addresses of function entry points; values are the addresses of all blocks contained in that function
  , rwsSections              :: !(Map.Map String (SectionInfo arch))
  -- ^ Information about the sections we inspected or produced
  }
deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (RewriterState arch)

data SectionInfo arch = SectionInfo { sectionStart, sectionEnd :: ConcreteAddress arch }
  deriving (Eq, Ord)
deriving instance MM.MemWidth (MM.ArchAddrWidth arch) => Show (SectionInfo arch)

-- | The base 'Monad' for the binary rewriter and relocation code.
--
-- It provides a source for symbolic addresses and provides error
-- handling facilities.
newtype RedirectT lm arch m a =
  RedirectT { unRedirectT :: RWS.RWST (RewriterEnv lm arch)
                             ()
                             (RewriterState arch)
                             m
                             a
            }
  deriving (Applicative,
            Functor,
            Monad,
            RWS.MonadReader (RewriterEnv lm arch),
            RWS.MonadState  (RewriterState arch),
            RWS.MonadWriter (),
            X.MonadThrow,
            T.MonadIO)

instance T.MonadTrans (RedirectT lm arch) where
  lift m = RedirectT $ do
    res <- RWS.RWST $ \_ s -> do
      a <- m
      return (a, s, mempty)
    return res

-- | The initial state of the 'Rewriter' 'Monad'
initialState :: RewriterState arch
initialState =  RewriterState
  { rwsNewSymbolsMap         = mempty
  , rwsBackwardBlockMapping  = mempty
  , rwsBlockMapping          = mempty
  , rwsFunctionBlocks        = mempty
  , rwsSections              = mempty
  }

-- | Run a 'RedirectT' computation.
--
-- It returns *all* diagnostics that occur before an exception is
-- thrown.
--
-- FIXME: This needs the set of input additional blocks that are allocated symbolic addresses
runRedirectT
  :: (T.MonadIO m)
  => LJ.LogAction IO (RCD.Diagnostic lm)
  -> ISA.ISA arch
  -> MM.Memory (MM.ArchAddrWidth arch)
  -> SymbolMap arch
  -> RedirectT lm arch m a
  -> m a
runRedirectT logAction isa mem symmap a = do
  (r, _s, ()) <- RWS.runRWST (unRedirectT a) env initialState
  return r
  where
    env = RewriterEnv { reISA = isa
                      , reMem = mem
                      , reSymbolMap = symmap
                      , reLogAction = logAction
                      }

-- | Log a diagnostic in the 'RedirectT' monad
logDiagnostic :: (T.MonadIO m) => RCD.Diagnostic lm -> RedirectT lm arch m ()
logDiagnostic d = do
  logAction <- RWS.asks reLogAction
  T.liftIO $ LJ.writeLog logAction d

-- | Read the 'ISA' from the 'RedirectT' environment
askISA :: (Monad m) => RedirectT lm arch m (ISA.ISA arch)
askISA = reISA <$> RWS.ask

askMem :: (Monad m) => RedirectT lm arch m (MM.Memory (MM.ArchAddrWidth arch))
askMem = reMem <$> RWS.ask

askSymbolMap :: (Monad m) => RedirectT lm arch m (SymbolMap arch)
askSymbolMap = reSymbolMap <$> RWS.ask

putNewSymbolsMap :: (Monad m) => NewSymbolsMap arch -> RedirectT lm arch m ()
putNewSymbolsMap symmap = do
  s <- RWS.get
  RWS.put $! s { rwsNewSymbolsMap = symmap }

getNewSymbolsMap :: (Monad m) => RedirectT lm arch m (NewSymbolsMap arch)
getNewSymbolsMap = do
  s <- RWS.get
  return $! rwsNewSymbolsMap s

