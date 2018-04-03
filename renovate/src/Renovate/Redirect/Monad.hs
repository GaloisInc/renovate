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
  checkRedirection,
  runRewriter,
  runRewriterT,
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

import qualified Data.Macaw.Memory as MM

import           Renovate.Address
import           Renovate.ISA
import           Renovate.Diagnostic

data SomeAddr a = Addr32 (a 32)
                | Addr64 (a 64)

deriving instance (Eq (a 32), Eq (a 64)) => Eq (SomeAddr a)
deriving instance (Ord (a 32), Ord (a 64)) => Ord (SomeAddr a)
deriving instance (Show (a 32), Show (a 64)) => Show (SomeAddr a)


type SymbolMap     w = Map (ConcreteAddress w) B.ByteString
type NewSymbolsMap w = Map (ConcreteAddress w) (ConcreteAddress w, B.ByteString)

data RewriterEnv i t w = RewriterEnv
  { reISA       :: !(ISA i t w)
  , reMem       :: !(MM.Memory w)
  , reSymbolMap :: !(SymbolMap w)
  }

data RewriterState w = RewriterState
  { rwsSymbolicAddressSource :: !Word64
  , rwsNewSymbolsMap         :: !(NewSymbolsMap w)
  , rwsUnrelocatableTerm     :: !Int
  -- ^ Count of blocks unrelocatable due to ending in an IP-relative indirect jump
  , rwsUnrelocatableSize     :: !Int
  -- ^ Count of blocks unrelocatable due to being too small to redirect
  , rwsReusedBytes           :: !Int
  -- ^ Count of bytes re-used by the compact layout strategy
  , rwsIncompleteBlocks      :: !Int
  -- ^ Count of blocks that are in incomplete functions
  , rwsBlockMapping          :: [(ConcreteAddress w, ConcreteAddress w)]
  -- ^ A mapping of original block addresses to the address they were redirected to
  }
  deriving (Show)

-- | The base 'Monad' for the binary rewriter and relocation code.
--
-- It provides a source for symbolic addresses and provides error
-- handling facilities.
newtype RewriterT i t w m a =
  RewriterT { unRewriterT :: ET.ExceptT E.SomeException
                                        (RWS.RWST (RewriterEnv i t w)
                                                  Diagnostics
                                                  (RewriterState w)
                                                  m)
                                         a
            }
  deriving (Applicative,
            Functor,
            Monad,
            RWS.MonadReader (RewriterEnv i t w),
            RWS.MonadState  (RewriterState w),
            RWS.MonadWriter Diagnostics,
            ET.MonadError E.SomeException)

-- | A 'RewriterT' over the 'I.Identity' 'Monad'
type Rewriter i t w a = RewriterT i t w I.Identity a

instance T.MonadTrans (RewriterT i t w) where
  lift m = RewriterT $ ET.ExceptT $ do
    res <- RWS.RWST $ \_ s -> do
      a <- m
      return (a, s, mempty)
    return (Right res)

-- | The initial state of the 'Rewriter' 'Monad'
initialState :: RewriterState w
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
data Redirection f w a =
  Redirection { rdBlocks :: f a
              , rdNewSymbols :: NewSymbolsMap w
              , rdBlockMapping :: [(ConcreteAddress w, ConcreteAddress w)]
              , rdDiagnostics :: [Diagnostic]
              , rdUnrelocatableTerm :: Int
              , rdSmallBlock :: Int
              , rdReusedBytes :: Int
              , rdIncompleteBlocks :: Int
              }

-- | Try to unwrap a 'Redirection'
--
-- If the 'Redirection' contains a 'Left', turn the whole thing into a 'Left'.
-- Otherwise, unwrap the 'Either' into an 'I.Identity'
checkRedirection :: Redirection (Either E.SomeException) w a
                 -> Either E.SomeException (Redirection I.Identity w a)
checkRedirection r =
  case rdBlocks r of
    Left exn -> Left exn
    Right a ->
      Right Redirection { rdBlocks = I.Identity a
                        , rdNewSymbols = rdNewSymbols r
                        , rdDiagnostics = rdDiagnostics r
                        , rdUnrelocatableTerm = rdUnrelocatableTerm r
                        , rdSmallBlock = rdSmallBlock r
                        , rdReusedBytes = rdReusedBytes r
                        , rdBlockMapping = rdBlockMapping r
                        , rdIncompleteBlocks = rdIncompleteBlocks r
                        }

-- | A wrapper around 'runReaderT' with 'I.Identity' as the base 'Monad'
runRewriter :: ISA i t w
            -> MM.Memory w
            -> SymbolMap w
            -> Rewriter i t w a
            -> Redirection (Either E.SomeException ) w a
runRewriter isa mem symmap a = I.runIdentity (runRewriterT isa mem symmap a)

-- | Run a 'RewriterT' computation.
--
-- It returns *all* diagnostics that occur before an exception is
-- thrown.
runRewriterT :: (Monad m)
             => ISA i t w
             -> MM.Memory w
             -> SymbolMap w
             -> RewriterT i t w m a
             -> m (Redirection (Either E.SomeException) w a)
runRewriterT isa mem symmap a = do
  (a', s, w) <- RWS.runRWST (ET.runExceptT (unRewriterT a)) (RewriterEnv isa mem symmap) initialState
  return Redirection { rdBlocks = a'
                     , rdNewSymbols = rwsNewSymbolsMap s
                     , rdDiagnostics = F.toList (diagnosticMessages w)
                     , rdUnrelocatableTerm = rwsUnrelocatableTerm s
                     , rdSmallBlock = rwsUnrelocatableSize s
                     , rdReusedBytes = rwsReusedBytes s
                     , rdBlockMapping = rwsBlockMapping s
                     , rdIncompleteBlocks = rwsIncompleteBlocks s
                     }

-- | Log a diagnostic in the 'RewriterT' monad
logDiagnostic :: (Monad m) => Diagnostic -> RewriterT i t w m ()
logDiagnostic = RWS.tell . Diagnostics . Seq.singleton

-- | Throw an error that halts the 'RewriterT' monad.
throwError :: (E.Exception e, Monad m) => e -> RewriterT i t w m a
throwError = ET.throwError . E.SomeException

-- | Return the next 'SymbolicAddress' that is available.
nextSymbolicAddress :: (Monad m) => RewriterT i t w m SymbolicAddress
nextSymbolicAddress = do
  addr <- RWS.gets rwsSymbolicAddressSource
  RWS.modify $ \s -> s { rwsSymbolicAddressSource = addr + 1 }
  return $ SymbolicAddress addr

-- | Read the 'ISA' from the 'RewriterT' environment
askISA :: (Monad m) => RewriterT i t w m (ISA i t w)
askISA = reISA <$> RWS.ask

askMem :: (Monad m) => RewriterT i t w m (MM.Memory w)
askMem = reMem <$> RWS.ask

askSymbolMap :: Monad m => RewriterT i t w m (SymbolMap w)
askSymbolMap = reSymbolMap <$> RWS.ask

putNewSymbolsMap :: Monad m => NewSymbolsMap w -> RewriterT i t w m ()
putNewSymbolsMap symmap = do
  s <- RWS.get
  RWS.put $! s { rwsNewSymbolsMap = symmap }

getNewSymbolsMap :: Monad m => RewriterT i t w m (NewSymbolsMap w)
getNewSymbolsMap = do
  s <- RWS.get
  return $! rwsNewSymbolsMap s

recordUnrelocatableTermBlock :: (Monad m) => RewriterT i t w m ()
recordUnrelocatableTermBlock = do
  s <- RWS.get
  RWS.put $! s { rwsUnrelocatableTerm = rwsUnrelocatableTerm s + 1 }

recordUnrelocatableSize :: (Monad m) => RewriterT i t w m ()
recordUnrelocatableSize = do
  s <- RWS.get
  RWS.put $! s { rwsUnrelocatableSize = rwsUnrelocatableSize s + 1 }

recordResuedBytes :: (Monad m) => Int -> RewriterT i t w m ()
recordResuedBytes nBytes = do
  s <- RWS.get
  RWS.put $! s { rwsReusedBytes = rwsReusedBytes s + nBytes }

recordBlockMap :: (Monad m) => [(ConcreteAddress w, ConcreteAddress w)] -> RewriterT i t w m ()
recordBlockMap m = do
  s <- RWS.get
  RWS.put $! s { rwsBlockMapping = m }

recordIncompleteBlock :: (Monad m) => RewriterT i t w m ()
recordIncompleteBlock = do
  s <- RWS.get
  RWS.put $! s { rwsIncompleteBlocks = rwsIncompleteBlocks s + 1 }
