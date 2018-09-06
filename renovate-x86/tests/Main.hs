module Main ( main ) where

import qualified Lang.Crucible.FunctionHandle as C
import           System.FilePath.Glob ( namesMatching )
import qualified Test.Tasty as T

import qualified X64 as T

main :: IO ()
main = do
  x64AsmTests <- namesMatching "tests/x64/*.s"
  hdlAlloc <- C.newHandleAllocator
  T.defaultMain $ T.testGroup "Renovate Tests" [
    T.x64Tests hdlAlloc x64AsmTests
    ]

