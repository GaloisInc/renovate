{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Inject (
  injectionAnalysis,
  ppc64Inject,
  injectionEquality
  ) where

import qualified Data.ByteString as BS
import           Data.Functor.Const ( Const(..) )
import qualified System.Exit as E
import qualified Test.Tasty.HUnit as T

import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Parameterized.List as PL
import qualified Dismantle.PPC as DP

import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP

injectionAnalysis :: BS.ByteString
                  -> (forall env . (R.HasAnalysisEnv env) => env arch binFmt -> Const () arch -> InjectedAddr arch -> R.SymbolicBlock arch -> R.RewriteM lm arch (Maybe ([R.TaggedInstruction arch (R.InstructionAnnotation arch)])))
                  -> R.AnalyzeAndRewrite lm arch binFmt (Const ())
injectionAnalysis injCode injRewrite =
  R.AnalyzeAndRewrite { R.arPreAnalyze = \_ -> return (Const ())
                      , R.arAnalyze = \_ _ -> return (Const ())
                      , R.arPreRewrite = injectPreRewrite injCode
                      , R.arRewrite = injRewrite
                      }

data InjectedAddr arch = InjectedAddr (R.SymbolicAddress arch)

injectPreRewrite :: (R.HasAnalysisEnv env) => BS.ByteString -> env arch binFmt -> b arch -> R.RewriteM lm arch (InjectedAddr arch)
injectPreRewrite injCode _ _ = do
  InjectedAddr <$> R.injectFunction "newExit" injCode

-- | This rewriter is PPC64-specific because it has to generate machine instructions
--
-- We'll need to add one per architecture
ppc64Inject :: (R.HasAnalysisEnv env)
            => env RP.PPC64 binFmt
            -> b RP.PPC64
            -> InjectedAddr RP.PPC64
            -> R.SymbolicBlock RP.PPC64
            -> R.RewriteM lm RP.PPC64 (Maybe ([R.TaggedInstruction RP.PPC64 (R.InstructionAnnotation RP.PPC64)]))
ppc64Inject env _ (InjectedAddr addr) sb = do
  return (Just (newCall ++ R.basicBlockInstructions sb))
  where
    mem = MBL.memoryImage (R.analysisLoadedBinary env)
    isa = R.analysisISA env
    callI = DP.Instruction DP.BL (DP.Calltarget (DP.BT 0) PL.:< PL.Nil)
    genI :: R.Instruction RP.PPC64 ()
    genI = R.fromGenericInstruction @RP.PPC64 callI
    newCall = R.isaSymbolizeAddresses isa mem (const Nothing) (R.concreteFromAbsolute 0) (Just addr) genI

-- | Instead of making sure the original and rewritten have the same behavior,
-- we want to make sure that the original binary fails and the new binary exits
-- with 0.
injectionEquality :: (E.ExitCode, E.ExitCode) -> (String, String) -> (String, String) -> IO ()
injectionEquality (origRC, modRC) _ _ = do
  case origRC of
    E.ExitSuccess -> T.assertFailure "Base binary succeeded"
    E.ExitFailure _ -> T.assertEqual "Expected the rewritten binary to succeed" E.ExitSuccess modRC
