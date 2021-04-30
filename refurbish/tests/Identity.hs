{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Identity (
  analysis,
  allOutputEqual
  ) where

import           Data.Functor.Const ( Const(..) )
import           Data.Parameterized.Context ( pattern Empty, pattern (:>) )
import qualified System.Exit as E
import           Test.Tasty.Checklist

import qualified Renovate as R

analysis :: R.AnalyzeAndRewrite lm arch binFmt (Const ())
analysis =
  R.AnalyzeAndRewrite { R.arPreAnalyze = \_ -> return (Const ())
                      , R.arAnalyze = \_ _ -> return (Const ())
                      , R.arPreRewrite = \_ _ -> return (Const ())
                      , R.arRewrite = R.identity
                      }

allOutputEqual :: (E.ExitCode, String, String) -> (E.ExitCode, String, String) -> IO ()
allOutputEqual origRes modRes =
  let rc (v, _, _) = v
      out (_, s, _) = s
      err (_, _, s) = s
  in withChecklist "equal results" $
     modRes `checkValues`
     (Empty
     :> Val "result code is the same" rc  (rc origRes)
     :> Val "stdout is the same"      out (out origRes)
     :> Val "stderr is the same"      err (err origRes)
     )

instance TestShow E.ExitCode where testShow = show
