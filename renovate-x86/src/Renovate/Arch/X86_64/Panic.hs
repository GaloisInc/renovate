{-# LANGUAGE TemplateHaskell #-}
module Renovate.Arch.X86_64.Panic (
  P.panic,
  RenovatePanic(..)
  ) where

import qualified Panic as P

data RenovatePanic = X86_64ISA
  deriving (Show)

instance P.PanicComponent RenovatePanic where
  panicComponentName pc = "Renovate[" ++ show pc ++ "]"
  panicComponentIssues _ = "https://github.com/GaloisInc/renovate/issues"
  panicComponentRevision = $(P.useGitRevision)
