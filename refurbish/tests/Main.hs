{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import           Control.DeepSeq ( force )
import qualified Control.Exception as X
import           Data.Bits ( (.|.) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ElfEdit as E
import qualified Data.Foldable as F
import           Data.Functor.Const ( Const(..) )
import qualified System.Directory as SD
import qualified System.Exit as E
import           System.FilePath ( (</>), (<.>) )
import           System.FilePath.Glob ( namesMatching )
import qualified System.IO as IO
import qualified System.IO.Temp as TMP
import qualified System.Posix.Files as SPF
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import           Text.Read ( readMaybe )

import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Macaw.Symbolic as MS
import           Data.Macaw.PPC.Symbolic ()
import           Data.Macaw.X86.Symbolic ()
import qualified Data.Parameterized.NatRepr as NR
import qualified Lang.Crucible.FunctionHandle as C

import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP
import qualified Renovate.Arch.X86_64 as RX

import qualified Refurbish.Docker as RD

import qualified Identity as RTId
import qualified Inject as RTIn

main :: IO ()
main = do
  eqemu <- RD.initializeQemuRunner
  mRunner <- case eqemu of
    Left (ec, out, err) -> do
      IO.hPutStrLn IO.stderr ("Failed to initialize qemu-runner container: " ++ show ec)
      IO.hPutStrLn IO.stderr out
      IO.hPutStrLn IO.stderr err
      return Nothing
    Right runner -> return (Just runner)
  exes <- namesMatching "tests/binaries/*.exe"
  hdlAlloc <- C.newHandleAllocator
  let injection = [ ("tests/injection-base/injection-base.ppc64.exe", "tests/injection-base/ppc64-exit.bin")]
  T.defaultMain $ T.testGroup "RefurbishTests" [
    rewritingTests mRunner hdlAlloc (R.LayoutStrategy R.Parallel R.BlockGrouping R.WholeFunctionTrampoline) exes,
    rewritingTests mRunner hdlAlloc (R.LayoutStrategy (R.Compact R.SortedOrder) R.BlockGrouping R.WholeFunctionTrampoline) exes,
    rewritingTests mRunner hdlAlloc (R.LayoutStrategy (R.Compact R.SortedOrder) R.LoopGrouping R.WholeFunctionTrampoline) exes,
    rewritingTests mRunner hdlAlloc (R.LayoutStrategy (R.Compact R.SortedOrder) R.FunctionGrouping R.WholeFunctionTrampoline) exes,
    codeInjectionTests mRunner hdlAlloc (R.LayoutStrategy R.Parallel R.BlockGrouping R.WholeFunctionTrampoline) injection,
    codeInjectionTests mRunner hdlAlloc (R.LayoutStrategy (R.Compact R.SortedOrder) R.BlockGrouping R.WholeFunctionTrampoline) injection
    ]

-- | Generate a set of tests for the code injection API
--
-- Right now, the one test is pretty simple: take a binary that just exits with
-- a non-zero exit code and use the code injection API to insert a call to a
-- function that instead makes it exit with 0.
codeInjectionTests :: Maybe RD.Runner
                   -> C.HandleAllocator
                   -> R.LayoutStrategy
                   -> [(FilePath, FilePath)]
                   -> T.TestTree
codeInjectionTests mRunner hdlAlloc strat exes =
  T.testGroup ("Injecting " ++ show strat) (map (toCodeInjectionTest mRunner hdlAlloc strat) exes)

toCodeInjectionTest :: Maybe RD.Runner
                    -> C.HandleAllocator
                    -> R.LayoutStrategy
                    -> (FilePath, FilePath)
                    -> T.TestTree
toCodeInjectionTest mRunner hdlAlloc strat (exePath, injectCodePath) = T.testCase exePath $ do
  ic <- BS.readFile injectCodePath
  let injAnalysis = RP.config64 (RTIn.injectionAnalysis ic RTIn.ppc64Inject)
  let configs = [ (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr injAnalysis)
                ]
  withELF exePath configs (testRewriter mRunner hdlAlloc strat exePath RTIn.injectionEquality)


-- | Generate a set of rewriting tests
--
-- If the runner is not 'Nothing', each test will use the runner to validate
-- that the executable still runs correctly
rewritingTests :: Maybe RD.Runner
               -> C.HandleAllocator
               -> R.LayoutStrategy
               -> [FilePath]
               -> T.TestTree
rewritingTests mRunner hdlAlloc strat exes =
  T.testGroup ("Rewriting " ++ show strat)
              (map (toRewritingTest mRunner hdlAlloc strat) exes)

toRewritingTest :: Maybe RD.Runner
                -> C.HandleAllocator
                -> R.LayoutStrategy
                -> FilePath
                -> T.TestTree
toRewritingTest mRunner hdlAlloc strat exePath = T.testCase exePath $ do
  let configs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 RTId.analysis))
                , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 RTId.analysis))
                , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config RTId.analysis))
                ]

  withELF exePath configs (testRewriter mRunner hdlAlloc strat exePath RTId.allOutputEqual)

testRewriter :: ( w ~ MM.ArchAddrWidth arch
                , E.ElfWidthConstraints w
                , MS.SymArchConstraints arch
                , R.InstructionConstraints arch
                , MBL.BinaryLoader arch (E.Elf w)
                )
             => Maybe RD.Runner
             -> C.HandleAllocator
             -> R.LayoutStrategy
             -> FilePath
             -> ((E.ExitCode, E.ExitCode) -> (String, String) -> (String, String) -> IO ())
             -> R.RenovateConfig arch (E.Elf w) (R.AnalyzeAndRewrite lm) (Const ())
             -> E.Elf w
             -> MBL.LoadedBinary arch (E.Elf w)
             -> IO ()
testRewriter mRunner hdlAlloc strat exePath assertions rc e loadedBinary = do
  (e', _, _) <- R.rewriteElf rc hdlAlloc e loadedBinary strat
  let !bs = force (E.renderElf e')
  T.assertBool "Invalid ELF length" (LBS.length bs > 0)
  -- If we have a runner available, compare the output of the original
  -- executable against the output of the rewritten executable.  We use argument
  -- lists provided by the test writer (or the empty argument list if none is
  -- specified).
  case mRunner of
    Nothing -> return ()
    Just runner -> do
      TMP.withSystemTempFile "refurbish.exe" $ \texe thdl -> do
        LBS.hPut thdl bs
        -- We have to close the handle so that it can be executed inside of the container
        IO.hClose thdl
        SPF.setFileMode texe (SPF.ownerModes .|. SPF.groupModes .|. SPF.otherModes)
        pwd <- SD.getCurrentDirectory
        argLists <- readTestArguments exePath
        -- The name of the executable mapped into the container
        let targetName = "/tmp/refurbish.exe"
        F.forM_ argLists $ \argList -> do
          (origRC, origOut, origErr) <- RD.runInContainer runner [(pwd </> exePath, targetName)] (targetName : argList)
          (modRC, modOut, modErr) <- RD.runInContainer runner [(texe, targetName)] (targetName : argList)
          assertions (origRC, modRC) (origOut, modOut) (origErr, modErr)

-- | Given a test executable, read the list of argument lists for a test executable
--
-- The executable will be run with each of the argument lists.  For a test
-- executable named "foo.exe", the argument list file is "foo.exe.args".  If the
-- file does not exist, the executable will be run with no arguments.  If the
-- file contains the empty list (of argument lists), the executable will be
-- rewritten but never run.
readTestArguments :: FilePath -> IO [[String]]
readTestArguments exePath = do
  mContents <- X.try (readFile (exePath <.> "args"))
  case mContents of
    Left (_ :: X.IOException) -> return [[]]
    Right contents ->
      case readMaybe contents of
        Just cs -> return cs
        Nothing -> T.assertFailure ("Unparsable argument contents: " ++ contents)


-- ELF handling

withELF :: FilePath
        -> [(R.Architecture, R.SomeConfig (R.AnalyzeAndRewrite lm) a)]
        -> (forall arch . ( MS.SymArchConstraints arch
                          , MBL.BinaryLoader arch (E.Elf (MM.ArchAddrWidth arch))
                          , E.ElfWidthConstraints (MM.ArchAddrWidth arch)
                          , R.InstructionConstraints arch
                          ) =>
               R.RenovateConfig arch (E.Elf (MM.ArchAddrWidth arch)) (R.AnalyzeAndRewrite lm) a
            -> E.Elf (MM.ArchAddrWidth arch)
            -> MBL.LoadedBinary arch (E.Elf (MM.ArchAddrWidth arch))
            -> IO ())
        -> T.Assertion
withELF exePath configs k = do
  bytes <- BS.readFile exePath
  case E.parseElf bytes of
    E.ElfHeaderError _ err -> T.assertFailure ("ELF header error: " ++ err)
    E.Elf32Res errs e32 -> do
      case errs of
        [] -> return ()
        _ -> T.assertFailure ("ELF32 errors: " ++ show errs)
      R.withElfConfig (E.Elf32 e32) configs k
    E.Elf64Res errs e64 -> do
      case errs of
        [] -> return ()
        _ -> T.assertFailure ("ELF64 errors: " ++ show errs)
      R.withElfConfig (E.Elf64 e64) configs k
