{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Inject (
  injectionAnalysis,
  ppc64Inject,
  injectionEquality
  ) where

import qualified Data.ByteString as BS
import           Data.Functor.Const ( Const(..) )
import qualified Data.List.NonEmpty as DLN
import           Data.Parameterized.Classes
import           Data.Parameterized.Context ( pattern Empty, pattern (:>) )
import           Data.Void ( Void )
import qualified System.Exit as E
import           Test.Tasty.Checklist

import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Parameterized.List as PL
import qualified Dismantle.PPC as DP

import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP

injectionAnalysis :: BS.ByteString
                  -> (forall env . (R.HasAnalysisEnv env) => env arch binFmt -> Const () arch -> InjectedAddr arch -> R.SymbolicBlock arch -> R.RewriteM Void arch (Maybe (R.ModifiedInstructions arch)))
                  -> R.AnalyzeAndRewrite Void arch binFmt (Const ())
injectionAnalysis injCode injRewrite =
  R.AnalyzeAndRewrite { R.arPreAnalyze = \_ -> return (Const ())
                      , R.arAnalyze = \_ _ -> return (Const ())
                      , R.arPreRewrite = injectPreRewrite injCode
                      , R.arRewrite = injRewrite
                      }

data InjectedAddr arch = InjectedAddr (R.SymbolicAddress arch)

injectPreRewrite :: (R.HasAnalysisEnv env) => BS.ByteString -> env arch binFmt -> b arch -> R.RewriteM Void arch (InjectedAddr arch)
injectPreRewrite injCode _ _ = do
  InjectedAddr <$> R.injectFunction "newExit" injCode

-- | This rewriter is PPC64-specific because it has to generate machine instructions
--
-- We'll need to add one per architecture
ppc64Inject :: forall env binFmt b
             . (R.HasAnalysisEnv env)
            => env RP.PPC64 binFmt
            -> b RP.PPC64
            -> InjectedAddr RP.PPC64
            -> R.SymbolicBlock RP.PPC64
            -> R.RewriteM Void RP.PPC64 (Maybe (R.ModifiedInstructions RP.PPC64))
ppc64Inject _env _ (InjectedAddr addr) sb = do
  R.withSymbolicInstructions sb $ \repr insns -> do
    case testEquality repr RP.PPCRepr of
      Nothing -> error "Unexpected impossible repr"
      Just Refl -> do
        let callI = DP.Instruction DP.BL (DP.Calltarget (DP.BT 0) PL.:< PL.Nil)
        let genI = R.fromGenericInstruction @RP.PPC64 RP.PPCRepr callI
        let toSymbolic :: forall s . DP.Annotated () DP.Operand s -> DP.Annotated (R.Relocation RP.PPC64) DP.Operand s
            toSymbolic (DP.Annotated _ op) =
              case op of
                DP.Calltarget (DP.BT _) -> DP.Annotated (R.SymbolicRelocation addr) op
                _ -> DP.Annotated R.NoRelocation op
        let newCall = RP.annotateInstrWith toSymbolic genI
        return (Just (R.ModifiedInstructions repr (newCall DLN.<| insns)))

-- | Instead of making sure the original and rewritten have the same behavior,
-- we want to make sure that the original binary fails and the new binary exits
-- with 0.
injectionEquality :: (E.ExitCode, String, String) -> (E.ExitCode, String, String) -> IO ()
injectionEquality origRes modRes =
  let origRC ((v, _, _), _) = v
      modRC (_, (v, _, _)) = v
      modErr (_, (_, _, s)) = s
  in withChecklist "injection execution results" $
     (origRes, modRes) `checkValues`
     (Empty
      :> Val "original binary fails"      ((E.ExitSuccess ==) . origRC) False
      :> Val "rewritten binary succeeds"  modRC  E.ExitSuccess
      :> Val "rewritten binary no errors" modErr ""
     )


instance TestShow E.ExitCode where testShow = show
