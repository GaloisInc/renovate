module Identity (
  analysis,
  allOutputEqual
  ) where

import           Data.Functor.Const ( Const(..) )
import qualified System.Exit as E
import qualified Test.Tasty.HUnit as T

import qualified Renovate as R

analysis :: R.AnalyzeAndRewrite lm arch binFmt (Const ())
analysis =
  R.AnalyzeAndRewrite { R.arPreAnalyze = \_ -> return (Const ())
                      , R.arAnalyze = \_ _ -> return (Const ())
                      , R.arPreRewrite = \_ _ -> return (Const ())
                      , R.arRewrite = R.identity
                      }

allOutputEqual :: (E.ExitCode, E.ExitCode) -> (String, String) -> (String, String) -> IO ()
allOutputEqual (origRC, modRC) (origOut, modOut) (origErr, modErr) = do
  T.assertEqual "Stdout" origOut modOut
  T.assertEqual "Stderr" origErr modErr
  T.assertEqual "Exit code" origRC modRC
