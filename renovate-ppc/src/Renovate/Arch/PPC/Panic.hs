{-# LANGUAGE TemplateHaskell #-}
module Renovate.Arch.PPC.Panic (
  P.panic,
  RenovatePanic(..)
  ) where

import qualified Panic as P

data RenovatePanic = PPCISA
  deriving (Show)

instance P.PanicComponent RenovatePanic where
  panicComponentName pc = "Renovate[" ++ show pc ++ "]"
  panicComponentIssues _ = "https://github.com/GaloisInc/renovate/issues"
  panicComponentRevision = $(P.useGitRevision)
