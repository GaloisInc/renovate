{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
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
  Redirection(..),
  RewriterState,
  Diagnostics,
  mkRedirection,
  runRewriter,
  runRewriterT,
  resumeRewriterT,
  throwError,
  logDiagnostic,
  nextSymbolicAddress,
  askISA,
  askMem,
  askSymbolMap,
  putNewSymbolsMap,
  getNewSymbolsMap,
  recordUnrelocatableTermBlock,
  recordIncompleteBlock,
  recordUnrelocatableSize,
  recordResuedBytes,
  recordBlockMap,
  ) where


import qualified Control.Monad.Catch as E
import qualified Control.Monad.Except as ET
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.Trans as T
import qualified Data.Foldable as F
import qualified Data.Functor.Identity as I
import qualified Data.ByteString as B
import           Data.Map ( Map )
import           Data.Monoid
import qualified Data.Sequence as Seq
import           Data.Word ( Word64 )

import           Prelude

import qualified Data.Macaw.CFG as MM

import           Renovate.Address
import           Renovate.ISA
import           Renovate.Diagnostic

data SomeAddr a = Addr32 (a 32)
                | Addr64 (a 64)

deriving instance (Eq (a 32), Eq (a 64)) => Eq (SomeAddr a)
deriving instance (Ord (a 32), Ord (a 64)) => Ord (SomeAddr a)
deriving instance (Show (a 32), Show (a 64)) => Show (SomeAddr a)


type SymbolMap     arch = Map (ConcreteAddress arch) B.ByteString
type NewSymbolsMap arch = Map (ConcreteAddress arch) (ConcreteAddress arch, B.ByteString)

data RewriterEnv arch = RewriterEnv
  { reISA       :: !(ISA arch)
  , reMem       :: !(MM.Memory (MM.ArchAddrWidth arch))
  , reSymbolMap :: !(SymbolMap arch)
  }

data RewriterState arch = RewriterState
  { rwsSymbolicAddressSource :: !Word64
  , rwsNewSymbolsMap         :: !(NewSymbolsMap arch)
  , rwsUnrelocatableTerm     :: !Int
  -- ^ Count of blocks unrelocatable due to ending in an IP-relative indirect jump
  , rwsUnrelocatableSize     :: !Int
  -- ^ Count of blocks unrelocatable due to being too small to redirect
  , rwsReusedBytes           :: !Int
  -- ^ Count of bytes re-used by the compact layout strategy
  , rwsIncompleteBlocks      :: !Int
  -- ^ Count of blocks that are in incomplete functions
  , rwsBlockMapping          :: [(ConcreteAddress arch, ConcreteAddress arch)]
  -- ^ A mapping of original block addresses to the address they were redirected to
  }

deriving instance (MM.MemWidth (MM.ArchAddrWidth arch)) => Show (RewriterState arch)

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
  { rwsSymbolicAddressSource = 0
  , rwsNewSymbolsMap         = mempty
  , rwsUnrelocatableTerm     = 0
  , rwsUnrelocatableSize     = 0
  , rwsReusedBytes           = 0
  , rwsIncompleteBlocks      = 0
  , rwsBlockMapping          = []
  }

-- | A type wrapping up the results of the 'Rewriter' Monad (runnable by
-- 'runRewriter' and 'runRewriterT').
data Redirection arch =
  Redirection { rdNewSymbols :: NewSymbolsMap arch
              , rdBlockMapping :: [(ConcreteAddress arch, ConcreteAddress arch)]
              , rdDiagnostics :: [Diagnostic]
              , rdUnrelocatableTerm :: Int
              , rdSmallBlock :: Int
              , rdReusedBytes :: Int
              , rdIncompleteBlocks :: Int
              }

-- | A wrapper around 'runRewriterT' with 'I.Identity' as the base 'Monad'
runRewriter :: ISA arch
            -> MM.Memory (MM.ArchAddrWidth arch)
            -> SymbolMap arch
            -> Rewriter arch a
            -> (Either E.SomeException a, RewriterState arch, Diagnostics)
runRewriter isa mem symmap a = I.runIdentity (runRewriterT isa mem symmap a)

-- | Run a 'RewriterT' computation.
--
-- It returns *all* diagnostics that occur before an exception is
-- thrown.
runRewriterT :: (Monad m)
             => ISA arch
             -> MM.Memory (MM.ArchAddrWidth arch)
             -> SymbolMap arch
             -> RewriterT arch m a
             -> m (Either E.SomeException a, RewriterState arch, Diagnostics)
runRewriterT isa mem symmap a = do
  RWS.runRWST (ET.runExceptT (unRewriterT a)) (RewriterEnv isa mem symmap) initialState

-- | Continue a 'RewriteT' computation using existing state and writer data.
resumeRewriterT  :: (Monad m)
                 => ISA arch
                 -> MM.Memory (MM.ArchAddrWidth arch)
                 -> SymbolMap arch
                 -> RewriterState arch
                 -> Diagnostics
                 -> RewriterT arch m a
                 -> m (Either E.SomeException a, RewriterState arch, Diagnostics)
resumeRewriterT isa mem symmap s0 w0 a = do
  (a', s1, w1) <- RWS.runRWST (ET.runExceptT (unRewriterT a)) (RewriterEnv isa mem symmap) s0
  return (a', s1, w0 <> w1)

-- | Turn the result of 'runRewriter' into a 'Redirection'.
--
-- We don't compute this is 'runRewriter' because making it separate
-- allows us to resume 'Rewriter' computations.
mkRedirection :: RewriterState arch -> Diagnostics -> Redirection arch
mkRedirection s w =
         Redirection { rdNewSymbols = rwsNewSymbolsMap s
                     , rdDiagnostics = F.toList (diagnosticMessages w)
                     , rdUnrelocatableTerm = rwsUnrelocatableTerm s
                     , rdSmallBlock = rwsUnrelocatableSize s
                     , rdReusedBytes = rwsReusedBytes s
                     , rdBlockMapping = rwsBlockMapping s
                     , rdIncompleteBlocks = rwsIncompleteBlocks s
                     }

-- | Log a diagnostic in the 'RewriterT' monad
logDiagnostic :: (Monad m) => Diagnostic -> RewriterT arch m ()
logDiagnostic = RWS.tell . Diagnostics . Seq.singleton

-- | Throw an error that halts the 'RewriterT' monad.
throwError :: (E.Exception e, Monad m) => e -> RewriterT arch m a
throwError = ET.throwError . E.SomeException

-- | Return the next 'SymbolicAddress' that is available.
nextSymbolicAddress :: (Monad m) => RewriterT arch m SymbolicAddress
nextSymbolicAddress = do
  addr <- RWS.gets rwsSymbolicAddressSource
  RWS.modify $ \s -> s { rwsSymbolicAddressSource = addr + 1 }
  return $ SymbolicAddress addr

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

recordUnrelocatableTermBlock :: (Monad m) => RewriterT arch m ()
recordUnrelocatableTermBlock = do
  s <- RWS.get
  RWS.put $! s { rwsUnrelocatableTerm = rwsUnrelocatableTerm s + 1 }

recordUnrelocatableSize :: (Monad m) => RewriterT arch m ()
recordUnrelocatableSize = do
  s <- RWS.get
  RWS.put $! s { rwsUnrelocatableSize = rwsUnrelocatableSize s + 1 }

recordResuedBytes :: (Monad m) => Int -> RewriterT arch m ()
recordResuedBytes nBytes = do
  s <- RWS.get
  RWS.put $! s { rwsReusedBytes = rwsReusedBytes s + nBytes }

recordBlockMap :: (Monad m) => [(ConcreteAddress arch, ConcreteAddress arch)] -> RewriterT arch m ()
recordBlockMap m = do
  s <- RWS.get
  RWS.put $! s { rwsBlockMapping = m }

recordIncompleteBlock :: (Monad m) => RewriterT arch m ()
recordIncompleteBlock = do
  s <- RWS.get
  RWS.put $! s { rwsIncompleteBlocks = rwsIncompleteBlocks s + 1 }
