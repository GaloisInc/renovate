{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main ( main ) where

import           Control.Concurrent.MVar
import           Control.DeepSeq ( force )
import qualified Control.Exception as X
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ElfEdit as E
import qualified Data.Foldable as F
import           Data.Functor.Const ( Const(..) )
import           Data.Functor.Contravariant ( (>$<) )
import           Data.Functor.Contravariant.Divisible ( chosen )
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text.IO as TIO
import           Data.Typeable ( Typeable )
import           GHC.TypeLits
import qualified Lumberjack as LJ
import qualified Prettyprinter as PD
import qualified Prettyprinter.Render.Text as PDT
import qualified System.Directory as SD
import qualified System.Exit as E
import           System.FilePath ( (</>), (<.>) )
import           System.FilePath.Glob ( namesMatching )
import qualified System.IO as IO
import qualified System.IO.Temp as TMP
import qualified System.Process as SP
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Test.Tasty.Options as TO
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
import qualified Renovate.Arch.AArch32 as RA

import qualified Refurbish.Docker as RD

import qualified Identity as RTId
import qualified Inject as RTIn

data UseDockerRunner = UseDockerRunner Bool
  deriving (Eq, Ord, Show)

instance TO.IsOption UseDockerRunner where
  defaultValue = UseDockerRunner True
  parseValue = fmap UseDockerRunner . TO.safeReadBool
  optionName = pure "no-docker-runner"
  optionHelp = pure "Do not the docker runner for each test binary"
  optionCLParser = TO.flagCLParser Nothing (UseDockerRunner False)

data VerboseOutput = VerboseOutput Bool
  deriving (Eq, Ord, Show)

instance TO.IsOption VerboseOutput where
  defaultValue = VerboseOutput False
  parseValue = fmap VerboseOutput . TO.safeReadBool
  optionName = pure "verbose-output"
  optionHelp = pure "Show verbose output during test runs"
  optionCLParser = TO.flagCLParser Nothing (VerboseOutput True)

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
  T.defaultMainWithIngredients ingredients $ do
    T.askOption $ \useDocker ->
      T.askOption $ \verbose -> do
      let callTest :: (UseDockerRunner -> VerboseOutput -> Maybe RD.Runner -> C.HandleAllocator
                       -> R.LayoutStrategy -> a -> T.TestTree)
                   -> R.LayoutStrategy -> a -> T.TestTree
          callTest f = f useDocker verbose mRunner hdlAlloc
      T.testGroup "RefurbishTests" [
        callTest rewritingTests (R.LayoutStrategy R.Parallel R.BlockGrouping R.AlwaysTrampoline) exes,
        callTest rewritingTests (R.LayoutStrategy R.Parallel R.BlockGrouping R.WholeFunctionTrampoline) exes,
        callTest rewritingTests (R.LayoutStrategy (R.Compact R.SortedOrder) R.BlockGrouping R.AlwaysTrampoline) exes,
        callTest rewritingTests (R.LayoutStrategy (R.Compact R.SortedOrder) R.LoopGrouping R.AlwaysTrampoline) exes,
        callTest rewritingTests (R.LayoutStrategy (R.Compact R.SortedOrder) R.FunctionGrouping R.WholeFunctionTrampoline) exes,
        callTest codeInjectionTests (R.LayoutStrategy R.Parallel R.BlockGrouping R.AlwaysTrampoline) injection,
        callTest codeInjectionTests (R.LayoutStrategy (R.Compact R.SortedOrder) R.BlockGrouping R.AlwaysTrampoline) injection
        ]
  where
    ingredients = T.includingOptions [ TO.Option (Proxy @UseDockerRunner)
                                     , TO.Option (Proxy @VerboseOutput)
                                     ] : T.defaultIngredients


-- | Generate a set of tests for the code injection API
--
-- Right now, the one test is pretty simple: take a binary that just exits with
-- a non-zero exit code and use the code injection API to insert a call to a
-- function that instead makes it exit with 0.
codeInjectionTests :: UseDockerRunner
                   -> VerboseOutput
                   -> Maybe RD.Runner
                   -> C.HandleAllocator
                   -> R.LayoutStrategy
                   -> [(FilePath, FilePath)]
                   -> T.TestTree
codeInjectionTests useDocker verbose mRunner hdlAlloc strat exes =
  T.testGroup ("Injecting " ++ show strat) (map (toCodeInjectionTest useDocker verbose mRunner hdlAlloc strat) exes)

toCodeInjectionTest :: UseDockerRunner
                    -> VerboseOutput
                    -> Maybe RD.Runner
                    -> C.HandleAllocator
                    -> R.LayoutStrategy
                    -> (FilePath, FilePath)
                    -> T.TestTree
toCodeInjectionTest useDocker verbose mRunner hdlAlloc strat (exePath, injectCodePath) = T.testCase exePath $ do
  ic <- BS.readFile injectCodePath
  let injAnalysis = RP.config64 (RTIn.injectionAnalysis ic RTIn.ppc64Inject)
  let configs = [ (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr injAnalysis)
                ]
  withELF exePath configs (testRewriter useDocker verbose mRunner hdlAlloc strat exePath RTIn.injectionEquality)


-- | Generate a set of rewriting tests
--
-- If the runner is not 'Nothing', each test will use the runner to validate
-- that the executable still runs correctly
rewritingTests :: UseDockerRunner
               -> VerboseOutput
               -> Maybe RD.Runner
               -> C.HandleAllocator
               -> R.LayoutStrategy
               -> [FilePath]
               -> T.TestTree
rewritingTests useDocker verbose mRunner hdlAlloc strat exes =
  T.testGroup ("Rewriting " ++ show strat)
              (map (toRewritingTest useDocker verbose mRunner hdlAlloc strat) exes)

toRewritingTest :: UseDockerRunner
                -> VerboseOutput
                -> Maybe RD.Runner
                -> C.HandleAllocator
                -> R.LayoutStrategy
                -> FilePath
                -> T.TestTree
toRewritingTest useDocker verbose mRunner hdlAlloc strat exePath = T.testCase exePath $ do
  let configs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 RTId.analysis))
                , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 RTId.analysis))
                , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config RTId.analysis))
                , (R.ARM, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RA.config RTId.analysis))
                ]

  withELF exePath configs (testRewriter useDocker verbose mRunner hdlAlloc strat exePath RTId.allOutputEqual)


-- | If verbose output, then log messages are always written to stdout.
-- If not verbose, they are only written if an exception is thrown
-- (i.e. the test fails).
withLogger :: VerboseOutput -> (LJ.LogAction IO (Either LJ.LogMessage R.Diagnostic) -> IO a) -> IO a
withLogger (VerboseOutput verbose) k =
  if verbose
  then k $ chosen
       (LJ.cvtLogMessageToANSITermText >$< LJ.LogAction TIO.putStrLn)
       (LJ.LogAction $ PDT.putDoc . (<> PD.line) . PD.pretty)
  else do mv <- newMVar []
          let ld = LJ.LogAction $ \msg -> modifyMVar_ mv (return . (Right (PD.pretty msg <> PD.line) :))
              lm = LJ.LogAction $ \msg -> modifyMVar_ mv (return . (Left (LJ.cvtLogMessageToANSITermText msg) :))
              l = chosen lm ld
          k l `X.catch`
            (\e -> do putStrLn ""
                      m <- reverse <$> readMVar mv
                      mapM_ (either TIO.putStrLn PDT.putDoc) m
                      putStrLn ""
                      X.throwIO (e :: X.SomeException))


testRewriter :: ( w ~ MM.ArchAddrWidth arch
                , E.ElfWidthConstraints w
                , MS.SymArchConstraints arch
                , R.ArchConstraints arch
                , Typeable arch
                , 16 <= w
                , MBL.BinaryLoader arch (E.ElfHeaderInfo w)
                )
             => UseDockerRunner
             -> VerboseOutput
             -> Maybe RD.Runner
             -> C.HandleAllocator
             -> R.LayoutStrategy
             -> FilePath
             -> ((E.ExitCode, E.ExitCode) -> (String, String) -> (String, String) -> IO ())
             -> R.RenovateConfig arch (E.ElfHeaderInfo w) (R.AnalyzeAndRewrite lm) (Const ())
             -> E.ElfHeaderInfo w
             -> MBL.LoadedBinary arch (E.ElfHeaderInfo w)
             -> IO ()
testRewriter (UseDockerRunner useDocker) verbose mRunner hdlAlloc strat exePath assertions rc e loadedBinary =
  withLogger verbose $ \l -> do
  (e', _, _, _) <- LJ.logFunctionCall (Left >$< l) "rewriteElf" $
                   R.rewriteElf (Right >$< l) rc hdlAlloc e loadedBinary strat
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
        p0 <- SD.getPermissions texe
        SD.setPermissions texe (SD.setOwnerExecutable True p0)
        pwd <- SD.getCurrentDirectory
        argLists <- readTestArguments exePath
        F.forM_ argLists $ \argList -> do
          let origTarget = pwd </> exePath
          (origRC, origOut, origErr) <- executor runner [(origTarget, origTarget)] (origTarget : argList)
          let newTarget = texe
          (modRC, modOut, modErr) <- executor runner [(newTarget, newTarget)] (newTarget : argList)
          assertions (origRC, modRC) (origOut, modOut) (origErr, modErr)
  where
    executor | useDocker = RD.runInContainer
             | otherwise = runWithoutContainer
    runWithoutContainer _ _ [] = error "Empty command line in the test runner"
    runWithoutContainer _runner _mappings (cmd:args) =
      SP.readProcessWithExitCode cmd args ""

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
                          , MBL.BinaryLoader arch (E.ElfHeaderInfo (MM.ArchAddrWidth arch))
                          , 16 <= MM.ArchAddrWidth arch
                          , Typeable arch
                          , E.ElfWidthConstraints (MM.ArchAddrWidth arch)
                          , R.ArchConstraints arch
                          ) =>
               R.RenovateConfig arch (E.ElfHeaderInfo (MM.ArchAddrWidth arch)) (R.AnalyzeAndRewrite lm) a
            -> E.ElfHeaderInfo (MM.ArchAddrWidth arch)
            -> MBL.LoadedBinary arch (E.ElfHeaderInfo (MM.ArchAddrWidth arch))
            -> IO ())
        -> T.Assertion
withELF exePath configs k = do
  bytes <- BS.readFile exePath
  case E.decodeElfHeaderInfo bytes of
    Left (byteOff, msg) -> T.assertFailure ("ELF parse error at " ++ show byteOff ++ ": " ++ msg)
    Right someHeader -> R.withElfConfig someHeader configs k
