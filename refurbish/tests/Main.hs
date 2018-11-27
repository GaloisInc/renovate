{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import           Control.DeepSeq ( force )
import qualified Control.Exception as X
import           Control.Monad.ST ( RealWorld )
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
import qualified System.Process as P
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import           Text.Read ( readMaybe )

import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Parameterized.List as PL
import qualified Data.Parameterized.NatRepr as NR
import qualified Dismantle.PPC as DP
import qualified Lang.Crucible.FunctionHandle as C

import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP
import qualified Renovate.Arch.X86_64 as RX

main :: IO ()
main = do
  eqemu <- initializeQemuRunner
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
    rewritingTests mRunner hdlAlloc R.Parallel exes,
    rewritingTests mRunner hdlAlloc (R.Compact R.SortedOrder) exes,
    codeInjectionTests mRunner hdlAlloc R.Parallel injection,
    codeInjectionTests mRunner hdlAlloc (R.Compact R.SortedOrder) injection
    ]

-- | Generate a set of tests for the code injection API
--
-- Right now, the one test is pretty simple: take a binary that just exits with
-- a non-zero exit code and use the code injection API to insert a call to a
-- function that instead makes it exit with 0.
codeInjectionTests :: Maybe Runner
                   -> C.HandleAllocator RealWorld
                   -> R.LayoutStrategy
                   -> [(FilePath, FilePath)]
                   -> T.TestTree
codeInjectionTests mRunner hdlAlloc strat exes =
  T.testGroup ("Injecting " ++ show strat) (map (toCodeInjectionTest mRunner hdlAlloc strat) exes)

toCodeInjectionTest :: Maybe Runner
                    -> C.HandleAllocator RealWorld
                    -> R.LayoutStrategy
                    -> (FilePath, FilePath)
                    -> T.TestTree
toCodeInjectionTest mRunner hdlAlloc strat (exePath, injectCodePath) = T.testCase exePath $ do
  ic <- BS.readFile injectCodePath
  let injAnalysis = RP.config64 (injectionAnalysis ic ppc64Inject)
  let configs = [ (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr injAnalysis)
                ]
  withELF exePath configs (testRewriter mRunner hdlAlloc strat exePath injectionEquality)

withELF :: FilePath
        -> [(R.Architecture, R.SomeConfig R.AnalyzeAndRewrite a)]
        -> (forall arch . (R.ArchBits arch,
                                  MBL.BinaryLoader arch (E.Elf (MM.ArchAddrWidth arch)),
                                  E.ElfWidthConstraints (MM.ArchAddrWidth arch),
                                  R.InstructionConstraints arch)
                                   => R.RenovateConfig arch (E.Elf (MM.ArchAddrWidth arch)) R.AnalyzeAndRewrite a
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


-- | Generate a set of rewriting tests
--
-- If the runner is not 'Nothing', each test will use the runner to validate
-- that the executable still runs correctly
rewritingTests :: Maybe Runner
               -> C.HandleAllocator RealWorld
               -> R.LayoutStrategy
               -> [FilePath]
               -> T.TestTree
rewritingTests mRunner hdlAlloc strat exes =
  T.testGroup ("Rewriting" ++ show strat)
              (map (toRewritingTest mRunner hdlAlloc strat) exes)

toRewritingTest :: Maybe Runner
                -> C.HandleAllocator RealWorld
                -> R.LayoutStrategy
                -> FilePath
                -> T.TestTree
toRewritingTest mRunner hdlAlloc strat exePath = T.testCase exePath $ do
  let configs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 analysis))
                , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 analysis))
                , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config analysis))
                ]

  withELF exePath configs (testRewriter mRunner hdlAlloc strat exePath allOutputEqual)

-- | Instead of making sure the original and rewritten have the same behavior,
-- we want to make sure that the original binary fails and the new binary exits
-- with 0.
injectionEquality :: (E.ExitCode, E.ExitCode) -> (String, String) -> (String, String) -> IO ()
injectionEquality (origRC, modRC) _ _ = do
  case origRC of
    E.ExitSuccess -> T.assertFailure "Base binary succeeded"
    E.ExitFailure _ -> T.assertEqual "Expected the rewritten binary to succeed" E.ExitSuccess modRC

allOutputEqual :: (E.ExitCode, E.ExitCode) -> (String, String) -> (String, String) -> IO ()
allOutputEqual (origRC, modRC) (origOut, modOut) (origErr, modErr) = do
  T.assertEqual "Stdout" origOut modOut
  T.assertEqual "Stderr" origErr modErr
  T.assertEqual "Exit code" origRC modRC

testRewriter :: ( w ~ MM.ArchAddrWidth arch
                , E.ElfWidthConstraints w
                , R.ArchBits arch
                , R.InstructionConstraints arch
                , MBL.BinaryLoader arch (E.Elf w)
                )
             => Maybe Runner
             -> C.HandleAllocator RealWorld
             -> R.LayoutStrategy
             -> FilePath
             -> ((E.ExitCode, E.ExitCode) -> (String, String) -> (String, String) -> IO ())
             -> R.RenovateConfig arch (E.Elf w) R.AnalyzeAndRewrite (Const ())
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
    Just (Runner runner) -> do
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
          (origRC, origOut, origErr) <- runner [(pwd </> exePath, targetName)] (targetName : argList)
          (modRC, modOut, modErr) <- runner [(texe, targetName)] (targetName : argList)
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

injectionAnalysis :: BS.ByteString
                  -> (forall env . (R.HasAnalysisEnv env) => env arch binFmt -> Const () arch -> InjectedAddr arch -> R.SymbolicBlock arch -> R.RewriteM arch (Maybe ([R.TaggedInstruction arch (R.InstructionAnnotation arch)])))
                  -> R.AnalyzeAndRewrite arch binFmt (Const ())
injectionAnalysis injCode injRewrite =
  R.AnalyzeAndRewrite { R.arPreAnalyze = \_ -> return (Const ())
                      , R.arAnalyze = \_ _ -> return (Const ())
                      , R.arPreRewrite = injectPreRewrite injCode
                      , R.arRewrite = injRewrite
                      }

data InjectedAddr arch = InjectedAddr (R.SymbolicAddress arch)

injectPreRewrite :: (R.HasAnalysisEnv env) => BS.ByteString -> env arch binFmt -> b arch -> R.RewriteM arch (InjectedAddr arch)
injectPreRewrite injCode _ _ = do
  InjectedAddr <$> R.injectFunction "newExit" injCode

ppc64Inject :: (R.HasAnalysisEnv env)
            => env RP.PPC64 binFmt
            -> b RP.PPC64
            -> InjectedAddr RP.PPC64
            -> R.SymbolicBlock RP.PPC64
            -> R.RewriteM RP.PPC64 (Maybe ([R.TaggedInstruction RP.PPC64 (R.InstructionAnnotation RP.PPC64)]))
ppc64Inject env _ (InjectedAddr addr) sb = do
  return (Just (newCall ++ R.basicBlockInstructions sb))
  where
    mem = MBL.memoryImage (R.analysisLoadedBinary env)
    isa = R.analysisISA env
    callI = DP.Instruction DP.BL (DP.Calltarget (DP.BT 0) PL.:< PL.Nil)
    genI :: R.Instruction RP.PPC64 ()
    genI = R.fromGenericInstruction @RP.PPC64 callI
    newCall = R.isaSymbolizeAddresses isa mem (const Nothing) (R.concreteFromAbsolute 0) (Just addr) genI

analysis :: R.AnalyzeAndRewrite arch binFmt (Const ())
analysis =
  R.AnalyzeAndRewrite { R.arPreAnalyze = \_ -> return (Const ())
                      , R.arAnalyze = \_ _ -> return (Const ())
                      , R.arPreRewrite = \_ _ -> return (Const ())
                      , R.arRewrite = \_ _ _ b -> return (Just (R.basicBlockInstructions b))
                      }

qemuRunnerName :: String
qemuRunnerName = "refurbish-qemu-runner"

initializeQemuRunner :: IO (Either (Int, String, String) Runner)
initializeQemuRunner = do
  (ec, out, err) <- P.readProcessWithExitCode "docker" ["build", "-t", qemuRunnerName, "."] ""
  case ec of
    E.ExitSuccess -> do return (Right (Runner runInContainer))
    E.ExitFailure rv -> return (Left (rv, out, err))

-- | * A mapping of local filenames to in-container filenames - all must be absolute
--   * Command line to run
newtype Runner = Runner ([(FilePath, FilePath)] -> [String] -> IO (E.ExitCode, String, String))

runInContainer :: [(FilePath, FilePath)] -> [String] -> IO (E.ExitCode, String, String)
runInContainer mapping args =
  P.readProcessWithExitCode "docker" dargs ""
  where
    argMap = [["-v", src ++ ":" ++ dst] | (src, dst) <- mapping]
    dargs = concat [ ["run"]
                   , concat argMap
                   , [qemuRunnerName]
                   , args
                   ]
