{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
-- | Tests for the x64 ABI
module X64 ( x64Tests ) where

import qualified Data.ByteString as B
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Void ( Void )
import           Data.Word ( Word64 )
import qualified Lumberjack as LJ
import qualified Prettyprinter as PD
import           System.FilePath ( (<.>), replaceExtension )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import           Text.Read ( readMaybe )

import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.Symbolic as MS
import           Data.Macaw.X86.Symbolic ()

import qualified Lang.Crucible.FunctionHandle as C

import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as R64

import qualified Data.ElfEdit as E

x64Tests :: C.HandleAllocator -> [FilePath] -> T.TestTree
x64Tests hdlAlloc asmFiles = T.testGroup "x64 ABI tests" (map (mkTest hdlAlloc) asmFiles)

{-

Things we can reasonably test:

* Number + size of discovered basic blocks.  Size can be in terms of
  both instructions and bytes

* Block start addresses

* Ensure that, after rewriting, the first instruction of each
  sufficiently-large basic block is a jump to another basic block.  To
  make this easier, augment RewriterInfo with the rewritten blocks
  (original and instrumented)

Strategy: glob up some assembly files in a known location.  Leave a
file side-by-side with each one that has test results.

Compile each .s file with 'gcc -nostdlib' and slurp in the resulting
binary with elf and flexdis.  Then run the redirection bits over it.

With the identity transformation, ensure that a rewritten binary still
runs.

-}

data ExpectedBlock = ExpectedBlock { addr :: Word64
                                   , byteCount :: Word64
                                   , insnCount :: Int
                                   }
                     deriving (Read, Show, Eq)

data ExpectedResult = ExpectedResult { expectedBlocks :: [ExpectedBlock]
                                     -- ^ Blocks that we expect to see
                                     , ignoreBlocks :: [Word64]
                                     -- ^ Garbage blocks that we want
                                     -- to ignore (artifacts of code
                                     -- discovery)
                                     }
                    deriving (Read, Show, Eq)

mkTest :: C.HandleAllocator -> FilePath -> T.TestTree
mkTest hdlAlloc fp = T.testCase fp $ withELF elfFilename testRewrite
  where
    elfFilename = replaceExtension fp "exe"
    testRewrite :: E.SomeElf E.ElfHeaderInfo -> IO ()
    testRewrite elf = do
      Just expected <- readMaybe <$> readFile (fp <.> "expected")
      let cfg = [(R.X86_64
                 , R.SomeConfig NR.knownNat MBL.Elf64Repr (R64.config (R.AnalyzeOnly (analysis expected))))]
      R.withElfConfig elf cfg (testBlockRecovery hdlAlloc)


analysis :: (Monad m, R.HasAnalysisEnv env, MM.MemWidth (MM.ArchAddrWidth arch))
         => ExpectedResult
         -> env arch binFmt
         -> m (TestConfig arch)
analysis expected env =
  return $ foldr go (TestCfg True []) (R.biBlocks (R.analysisBlockInfo env))
  where
    go b inp@(TestCfg _bacc sacc) =
      R.withConcreteInstructions b $ \_repr insns ->
        let actual = ExpectedBlock { addr = fromIntegral (R.absoluteAddress (R.concreteBlockAddress b))
                                   , byteCount = R.blockSize (R.analysisISA env) b
                                   , insnCount = length insns
                                   }
            blockStr = unlines (F.toList (fmap (R.isaPrettyInstruction (R.analysisISA env)) insns))
        in case M.lookup (addr actual) expectedMap of
          Nothing
            | S.member (addr actual) ignoreSet -> inp
            | otherwise -> TestCfg False (("Unexpected block: " ++ show actual ++ " with instructions\n" ++ blockStr):sacc)
          Just eb -> case eb == actual of
                     True  -> inp
                     False -> TestCfg False $ ("Block mismatch:\n" ++ blockStr):sacc
    expectedMap = M.fromList [ (addr eb, eb) | eb <- expectedBlocks expected ]
    ignoreSet = S.fromList (ignoreBlocks expected)

data TestConfig a = TestCfg Bool [String]

testBlockRecovery :: ( w ~ MM.ArchAddrWidth arch
                     , R.ArchConstraints arch
                     , MBL.BinaryLoader arch binFmt
                     , E.ElfWidthConstraints w
                     , MS.SymArchConstraints arch
                     , 16 NR.<= w
                     )
                  => C.HandleAllocator
                  -> R.RenovateConfig arch binFmt R.AnalyzeOnly TestConfig
                  -> E.ElfHeaderInfo w
                  -> MBL.LoadedBinary arch binFmt
                  -> T.Assertion
testBlockRecovery hdlAlloc rc elf loadedBinary = do
  let logger :: LJ.LogAction IO (R.Diagnostic Void)
      logger = LJ.LogAction $ putStrLn . show . PD.pretty
  TestCfg status msgs <- R.analyzeElf logger rc hdlAlloc elf loadedBinary
  T.assertBool (unlines ("Analysis Failed:" : msgs)) status

withELF :: FilePath -> (E.SomeElf E.ElfHeaderInfo -> IO ()) -> IO ()
withELF fp k = do
  bytes <- B.readFile fp
  case E.decodeElfHeaderInfo bytes of
    Left (off, msg) ->
      error ("Error parsing ELF header at offset " ++ show off ++ ": " ++ msg)
    Right someElfHdr -> k someElfHdr
