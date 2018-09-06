{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import           Control.DeepSeq ( force )
import           Control.Monad.ST ( RealWorld )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ElfEdit as E
import           Data.Functor.Const ( Const(..) )
import           System.FilePath.Glob ( namesMatching )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Parameterized.NatRepr as NR
import qualified Lang.Crucible.FunctionHandle as C

import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP
import qualified Renovate.Arch.X86_64 as RX

main :: IO ()
main = do
  exes <- namesMatching "tests/binaries/*.exe"
  hdlAlloc <- C.newHandleAllocator
  T.defaultMain $ T.testGroup "RefurbishTests" [
    rewritingTests hdlAlloc R.Parallel exes
    ]

rewritingTests :: C.HandleAllocator RealWorld -> R.LayoutStrategy -> [FilePath]
               -> T.TestTree
rewritingTests hdlAlloc strat exes =
  T.testGroup ("Rewriting" ++ show strat)
              (map (toRewritingTest hdlAlloc strat) exes)

toRewritingTest :: C.HandleAllocator RealWorld -> R.LayoutStrategy -> FilePath
                -> T.TestTree
toRewritingTest hdlAlloc strat exePath = T.testCase exePath $ do
  bytes <- BS.readFile exePath
  let configs :: [(R.Architecture, R.SomeConfig R.TrivialConfigConstraint (Const ()))]
      configs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 analyze rewrite))
                , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 analyze rewrite))
                , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config analyze rewrite))
                ]
  case E.parseElf bytes of
    E.ElfHeaderError _ err -> T.assertFailure ("ELF header error: " ++ err)
    E.Elf32Res errs e32 -> do
      case errs of
        [] -> return ()
        _ -> T.assertFailure ("ELF32 errors: " ++ show errs)
      R.withElfConfig (E.Elf32 e32) configs (testRewriter hdlAlloc strat)
    E.Elf64Res errs e64 -> do
      case errs of
        [] -> return ()
        _ -> T.assertFailure ("ELF64 errors: " ++ show errs)
      R.withElfConfig (E.Elf64 e64) configs (testRewriter hdlAlloc strat)

testRewriter :: ( w ~ MM.ArchAddrWidth arch
                , E.ElfWidthConstraints w
                , R.ArchBits arch
                , R.InstructionConstraints arch
                , MBL.BinaryLoader arch (E.Elf w)
                )
             => C.HandleAllocator RealWorld
             -> R.LayoutStrategy
             -> R.RenovateConfig arch (E.Elf w) (Const ())
             -> E.Elf w
             -> MBL.LoadedBinary arch (E.Elf w)
             -> IO ()
testRewriter hdlAlloc strat rc e loadedBinary = do
  (e', _, _) <- R.rewriteElf rc hdlAlloc e loadedBinary strat
  let !bs = force (E.renderElf e')
  T.assertBool "Invalid ELF length" (LBS.length bs > 0)

analyze :: R.AnalyzeEnv arch -> MBL.LoadedBinary arch binFmt -> IO (Const () arch)
analyze _ _ = return (Const ())

rewrite :: Const () arch
        -> MBL.LoadedBinary arch binFmt
        -> R.SymbolicBlock arch
        -> R.RewriteM arch (Maybe [R.TaggedInstruction arch (R.InstructionAnnotation arch)])
rewrite _ _ b = return (Just (R.basicBlockInstructions b))


