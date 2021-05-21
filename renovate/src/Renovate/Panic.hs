{-# LANGUAGE TemplateHaskell #-}
module Renovate.Panic (
  RenovatePanic(..),
  P.panic
  ) where

import qualified Panic as P

data RenovatePanic = Layout
                   | BasicBlockSize
                   | Concretize
                   | Symbolize
                   | ELFWriting
                   | Assemble
  deriving (Show)

instance P.PanicComponent RenovatePanic where
  panicComponentName pc = "Renovate[" ++ show pc ++ "]"
  panicComponentIssues _ = "https://github.com/GaloisInc/renovate/issues"
  panicComponentRevision = $(P.useGitRevision)
