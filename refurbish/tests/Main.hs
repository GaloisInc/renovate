{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.List as L
import           Data.Proxy ( Proxy(..) )
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           Data.Typeable ( Typeable )
import           Data.Void ( Void )
import           GHC.Natural ( Natural )
import           GHC.TypeLits
import           Lumberjack ( (|#) )
import qualified Lumberjack as LJ
import qualified Prettyprinter as PD
import qualified Prettyprinter.Render.Text as PDT
import qualified System.Directory as SD
import qualified System.Exit as E
import           System.FilePath ( (</>), (<.>), replaceFileName, takeFileName )
import qualified System.IO as IO
import qualified System.IO.Temp as TMP
import qualified System.Process as SP
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import qualified Test.Tasty.Options as TO
import qualified Test.Tasty.Sugar as TS
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
import qualified Refurbish.QEMU as Q

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


rewriteCube :: TS.CUBE
rewriteCube =
  TS.mkCUBE { TS.inputDir = "tests/binaries"
            , TS.rootName = "*.c"
            , TS.expectedSuffix = "exe"
            , TS.validParams =
                [ ("cpu", Just [ "aarch32", "x86_64", "ppc64" ])
                , ("lib", Just [ "glibc", "musl", "stdlib", "nostdlib" ] )
                , ("opt", Just [ "opt", "noopt" ])
                ]
            , TS.associatedNames = [ ("arguments", "args") ]
            }

injectionCube :: TS.CUBE
injectionCube =
  TS.mkCUBE { TS.inputDir = "tests/injection-base"
            , TS.rootName = "*.c"
            , TS.expectedSuffix = "exe"
            , TS.validParams =
                [ ("cpu", Just [ "aarch32", "x86_64", "ppc64" ])
                ]
            , TS.associatedNames = [ ("arguments", "args") ]
            }


main :: IO ()
main = do
  rewriteSweets <- TS.findSugar rewriteCube
  injectionSweets <- TS.findSugar injectionCube
  hdlAlloc <- C.newHandleAllocator
  rewriteTests <- TS.withSugarGroups rewriteSweets
                  Sachet (mkRewriteTests hdlAlloc)
  injectionTests <- TS.withSugarGroups injectionSweets
                    Sachet (mkInjectionTests hdlAlloc)
  T.defaultMainWithIngredients ingredients $ do
    T.askOption $ \useDocker ->
      T.askOption $ \verbose ->
      withExecutor verbose useDocker $
      \e -> T.testGroup "Refurbish Tests"
            [ T.testGroup "Rewriting" $ sweeten e <$> rewriteTests
            , T.testGroup "Injection" $ sweeten e <$> injectionTests
            ]
  where
    ingredients = T.includingOptions (TO.Option (Proxy @UseDockerRunner)
                                      : TO.Option (Proxy @VerboseOutput)
                                       : TS.sugarOptions)
                  : TS.sugarIngredients [rewriteCube, injectionCube]
                  <> T.defaultIngredients

----------------------------------------------------------------------

data Sweetener = Sweetener (IO (Logger, Executor) -> T.TestTree)
               | Sachet String [Sweetener]

sweeten :: IO (Logger, Executor) -> Sweetener -> T.TestTree
sweeten mkExecutor (Sweetener s) = s mkExecutor
sweeten mkExecutor (Sachet nm s) = T.testGroup nm (sweeten mkExecutor <$> s)

strategies :: [R.LayoutStrategy]
strategies = [ R.LayoutStrategy allocator grouping trampoline
             | allocator <- [ R.Parallel
                            , R.Compact R.SortedOrder
                            ]
             , grouping <- [ R.BlockGrouping
                           , R.LoopGrouping
                           , R.FunctionGrouping
                           ]
             , trampoline <- [ R.WholeFunctionTrampoline
                             , R.AlwaysTrampoline
                             ]
             ]

mkRewriteTests :: C.HandleAllocator -> TS.Sweets -> Natural -> TS.Expectation
               -> IO [Sweetener]
mkRewriteTests hdlAlloc _sweets _testNum expect = return $
  if lookup "opt" (TS.expParamsMatch expect) == Just (TS.Assumed "noopt")
  then []  -- let the opt=opt test suffice for this .exe
  else let exe = TS.expectedFile expect
       in (Sweetener . toRewritingTest hdlAlloc exe) <$> strategies

mkInjectionTests :: C.HandleAllocator -> TS.Sweets -> Natural -> TS.Expectation
                 -> IO [Sweetener]
mkInjectionTests hdlAlloc _sweets _testNum expect = return $
  case lookup "cpu" (TS.expParamsMatch expect) of
    Just (TS.Explicit cpu) ->
      let exe = TS.expectedFile expect
          inj = replaceFileName exe $ cpu <> "-exit.bin"
      in (Sweetener . toCodeInjectionTest hdlAlloc exe inj <$> strategies)
    _ -> []

----------------------------------------------------------------------


-- | Generate a set of tests for the code injection API
--
-- Right now, the one test is pretty simple: take a binary that just exits with
-- a non-zero exit code and use the code injection API to insert a call to a
-- function that instead makes it exit with 0.

toCodeInjectionTest :: C.HandleAllocator
                    -> FilePath
                    -> FilePath
                    -> R.LayoutStrategy
                    -> IO (Logger, Executor)
                    -> T.TestTree
toCodeInjectionTest hdlAlloc exePath injectCodePath strat mkExecutor =
  T.testCaseSteps (L.intercalate ", " [ takeFileName exePath
                                      , show (R.allocator strat)
                                      , show (R.grouping strat)
                                      , show (R.trampolines strat)
                                      ]) $ \step -> do
  ic <- BS.readFile injectCodePath
  let injAnalysis = RP.config64 (RTIn.injectionAnalysis ic RTIn.ppc64Inject)
  let configs = [ (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr injAnalysis)
                ]
  withELF exePath configs (testRewriter mkExecutor step hdlAlloc strat exePath RTIn.injectionEquality)


-- | Generate a set of rewriting tests
--
-- If the runner is not 'Nothing', each test will use the runner to validate
-- that the executable still runs correctly

toRewritingTest :: C.HandleAllocator
                -> FilePath
                -> R.LayoutStrategy
                -> IO (Logger, Executor)
                -> T.TestTree
toRewritingTest hdlAlloc exePath strat mkExecutor =
  T.testCaseSteps (L.intercalate ", " [ takeFileName exePath
                                      , show (R.allocator strat)
                                      , show (R.grouping strat)
                                      , show (R.trampolines strat)
                                      ]) $ \step -> do
  let configs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 RTId.analysis))
                , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 RTId.analysis))
                , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config RTId.analysis))
                , (R.ARM, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RA.config RTId.analysis))
                ]

  withELF exePath configs (testRewriter mkExecutor step hdlAlloc strat exePath RTId.allOutputEqual)


testRewriter :: ( w ~ MM.ArchAddrWidth arch
                , E.ElfWidthConstraints w
                , MS.SymArchConstraints arch
                , R.ArchConstraints arch
                , Typeable arch
                , 16 <= w
                , MBL.BinaryLoader arch (E.ElfHeaderInfo w)
                )
             => IO (Logger, Executor)
             -> (String -> IO ())
             -> C.HandleAllocator
             -> R.LayoutStrategy
             -> FilePath
             -> ((E.ExitCode, String, String) -> (E.ExitCode, String, String) -> IO ())
             -> R.RenovateConfig arch (E.ElfHeaderInfo w) (R.AnalyzeAndRewrite lm) (Const ())
             -> E.ElfHeaderInfo w
             -> MBL.LoadedBinary arch (E.ElfHeaderInfo w)
             -> IO ()
testRewriter mkExecutor step hdlAlloc strat exePath assertions rc e loadedBinary = do
  (logger, executor) <- mkExecutor
  withLogger logger $ \l -> do
    let lm = Left >$< l
    step "rewrite executable"
    (e', _, _, _) <- LJ.logFunctionCall lm "rewriteElf" $
                     R.rewriteElf (Right >$< l) rc hdlAlloc e loadedBinary strat
    let !bs = force (E.renderElf e')
    T.assertBool "Invalid ELF length" (LBS.length bs > 0)
    -- If we have a runner available, compare the output of the original
    -- executable against the output of the rewritten executable.  We use argument
    -- lists provided by the test writer (or the empty argument list if none is
    -- specified).
    TMP.withSystemTempFile "refurbish.exe" $ \texe thdl -> do
      LBS.hPut thdl bs
      -- We have to close the handle so that it can be executed inside of the container
      IO.hClose thdl
      pwd <- SD.getCurrentDirectory
      argLists <- readTestArguments exePath
      F.forM_ argLists $ \argList -> do
        let orig = pwd </> exePath
        step "execute original"
        origres <- LJ.logFunctionCall lm "run original exe" $ executor orig argList
        step "execute rewritten"
        modres <- LJ.logFunctionCall lm "run rewritten exe" $ executor texe argList
        assertions origres modres


type Executor = (FilePath -> [String] -> IO (E.ExitCode, String, String))

withExecutor :: VerboseOutput
             -> UseDockerRunner
             -> (IO (Logger, Executor) -> T.TestTree)
             -> T.TestTree
withExecutor verbose (UseDockerRunner useDocker) k =
  let chmodExec f = SD.setPermissions f .
                    SD.setOwnerExecutable True
                    =<< SD.getPermissions f
  in
  if useDocker
  then T.withResource RD.initializeQemuRunner (const $ return ()) $
       \r -> let mkExecutor = do
                   l <- mkLogger verbose
                   r >>= \case
                     Left (ec, out, err) ->
                       return (l,
                               \_ _ -> T.assertFailure $ unlines
                                       [ "Could not initialize Docker QEMU runner (" <>
                                         show ec <> ")\n"
                                       , "STDOUT: " <> out
                                       , "STDERR: " <> err
                                       ])
                     Right runner ->
                       let executor exepath argList = do
                               chmodExec exepath
                               let dockerTgt = "/tmp" </> takeFileName exepath
                               let lw = let TestLogger (_, lw') = l in Left >$< lw'
                               RD.runInContainer runner lw
                                 [(exepath, dockerTgt)]
                                 (dockerTgt : argList)
                       in return (l, executor)
             in k mkExecutor
  else k (do l <- mkLogger verbose
             let executor exepath argList = do
                   chmodExec exepath
                   q <- Q.qemulator exepath
                   SD.findExecutable q >>= \case
                     Just qp -> do
                       let cmd = exepath : argList
                       let lw = let TestLogger (_, lw') = l in Left >$< lw'
                       LJ.writeLog lw |# "running: " <> (Text.pack $ unwords $ qp : cmd)
                       r <- SP.readProcessWithExitCode qp cmd ""
                       LJ.writeLog lw |# "  --> " <> LJ.tshow r
                       return r
                     Nothing ->
                       T.assertFailure $
                       "QEMU emulator " <> q <> " not found to run binary."
             return (l, executor)
         )


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
        Nothing -> T.assertFailure ("Unparseable argument contents: " ++ contents)


----------------------------------------------------------------------
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

----------------------------------------------------------------------
-- Logging

-- | If verbose output, then log messages are always written to stdout.
-- If not verbose, they are only written if an exception is thrown
-- (i.e. the test fails).

mkLogger :: VerboseOutput -> IO Logger
mkLogger (VerboseOutput verbose) =
  if verbose
  then let l = chosen
               (LJ.cvtLogMessageToANSITermText >$< LJ.LogAction TIO.putStrLn)
               (LJ.LogAction $ PDT.putDoc . (<> PD.line) . PD.pretty)
       in return $ TestLogger (Nothing, l)
  else do mv <- newMVar []
          let ld = LJ.LogAction $ \msg ->
                   modifyMVar_ mv (return . (Right (PD.pretty msg <> PD.line) :))
              lm = LJ.LogAction $ \msg ->
                   modifyMVar_ mv (return . (Left (LJ.cvtLogMessageToANSITermText msg) :))
          return $ TestLogger (Just mv, chosen lm ld)

withLogger :: Logger -> (LogWriter -> IO a) -> IO a
withLogger logger k =
  case logger of
    TestLogger (Nothing, l) -> k l
    TestLogger (Just mv, l) -> k l `X.catch`
                               (\e -> do putStrLn ""
                                         m <- reverse <$> readMVar mv
                                         mapM_ (either TIO.putStrLn PDT.putDoc) m
                                         putStrLn ""
                                         X.throwIO (e :: X.SomeException))

type LogWriter = LJ.LogAction IO (Either LJ.LogMessage R.Diagnostic)
newtype Logger = TestLogger (Maybe (MVar [Either Text.Text (PD.Doc Void)]), LogWriter)
