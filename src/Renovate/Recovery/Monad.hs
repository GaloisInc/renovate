{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | A Monad for 'BasicBlock' recovery (i.e., code discovery)
--
-- One day, this should be replaced by the Macaw library
module Renovate.Recovery.Monad (
  Diagnostic(..),
  Recovery,
  RecoveryT,
  runRecovery,
  runRecoveryT,
  throwError,
  logDiagnostic,
  askISA
  ) where

import           Control.Applicative
import           Control.Arrow ( second )
import qualified Control.Monad.Catch as E
import qualified Control.Monad.Except as ET
import qualified Control.Monad.RWS.Strict as RWS
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.Functor.Identity as I
import qualified Data.Sequence as Seq

import           Prelude

import qualified Data.Macaw.Memory as MC

import           Renovate.ISA
import           Renovate.Diagnostic

data RecoveryEnv i t w =
  RecoveryEnv { reMemory :: MC.Memory w
              , reISA :: ISA i t w
              , reDecoder :: B.ByteString -> Either E.SomeException (Int, i ())
              }

-- | The 'Monad' backing the code discovery algorithm.
--
-- It tracks an environment with things like the active ISA and
-- decoder.  It also records the discovered blocks.
newtype RecoveryT m i t w a = RecoveryT { unRecoveryT :: ET.ExceptT E.SomeException (RWS.RWST (RecoveryEnv i t w) Diagnostics () m) a }
                            deriving (Applicative,
                                      Functor,
                                      Monad,
                                      ET.MonadError E.SomeException,
                                      RWS.MonadState (),
                                      RWS.MonadReader (RecoveryEnv i t w),
                                      RWS.MonadWriter Diagnostics)

instance (Monad m) => E.MonadThrow (RecoveryT m i t w) where
  throwM = throwError

-- | The 'RecoveryT' 'Monad' instantiated to 'I.Identity'
type Recovery i t w a = RecoveryT I.Identity i t w a

-- | A wrapper around 'runRecoveryT' with 'I.Identity' as the base 'Monad'
runRecovery :: ISA i t w
            -> (forall m . (E.MonadThrow m) => B.ByteString -> m (Int, i ()))
            -> MC.Memory w
            -> Recovery i t w a
            -> (Either E.SomeException a, [Diagnostic])
runRecovery isa decoder mem a = I.runIdentity (runRecoveryT isa decoder mem a)

-- | Run a 'RecoveryT' computation.
--
-- It returns *all* diagnostics that occur before an exception is
-- thrown.
runRecoveryT :: (Monad m)
             => ISA i t w
             -> (forall m' . (E.MonadThrow m') => B.ByteString -> m' (Int, i ()))
             -> MC.Memory w
             -> RecoveryT m i t w a
             -> m (Either E.SomeException a, [Diagnostic])
runRecoveryT isa decoder mem a = do
  second (F.toList . diagnosticMessages) <$> RWS.evalRWST (ET.runExceptT (unRecoveryT a)) env ()
  where
    env = RecoveryEnv { reISA = isa
                      , reMemory = mem
                      , reDecoder = decoder
                      }

-- | Log a diagnostic in the 'RecoveryT' monad
logDiagnostic :: (Monad m) => Diagnostic -> RecoveryT m i t w ()
logDiagnostic = RWS.tell . Diagnostics . Seq.singleton

-- | Throw an error that halts the 'RecoveryT' monad.
throwError :: (E.Exception e, Monad m) => e -> RecoveryT m i t w a
throwError = ET.throwError . E.SomeException

-- | Read the 'ISA' from the 'RecoveryT' environment
askISA :: (Monad m) => RecoveryT m i t w (ISA i t w)
askISA = RWS.asks reISA

