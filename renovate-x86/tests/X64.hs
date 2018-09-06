{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Tests for the x64 ABI
module X64 ( x64Tests ) where

import           GHC.TypeLits ( KnownNat )
import           Data.Typeable ( Typeable )

import           Control.Monad.ST ( RealWorld )
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word ( Word64 )
import System.FilePath ( (<.>), replaceExtension )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Text.Read ( readMaybe )

import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Macaw.CFG as MM

import qualified Lang.Crucible.FunctionHandle as C

import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as R64

import qualified Data.ElfEdit as E

x64Tests :: C.HandleAllocator RealWorld -> [FilePath] -> T.TestTree
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

class (KnownNat (MM.ArchAddrWidth arch)) => TestConstraint arch (b :: * -> *) where
instance TestConstraint R64.X86_64 b

mkTest :: C.HandleAllocator RealWorld -> FilePath -> T.TestTree
mkTest hdlAlloc fp = T.testCase fp $ withELF elfFilename testRewrite
  where
    testRewrite :: E.Elf 64 -> IO ()
    testRewrite elf = do
      Just expected <- readMaybe <$> readFile (fp <.> "expected")
      let cfg :: [(R.Architecture, R.SomeConfig TestConstraint TestConfig)]
          cfg = [(R.X86_64, R.SomeConfig NR.knownNat MBL.Elf64Repr (R64.config (analysis expected) R.identity))]
      R.withElfConfig (E.Elf64 elf) cfg (testBlockRecovery hdlAlloc)

    elfFilename = replaceExtension fp "exe"

analysis :: Monad m
         => ExpectedResult -> R.AnalyzeEnv R64.X86_64 -> MBL.LoadedBinary R64.X86_64 binFmt
         -> m (TestConfig R64.X86_64)
analysis expected analyzeEnv _mem =
  return $ foldr go (TestCfg True []) (R.biBlocks $ R.envBlockInfo env)
  where
    env = R.aeRewriteEnv analyzeEnv
    go :: R.ConcreteBlock R64.X86_64 -> TestConfig R64.X86_64 -> TestConfig R64.X86_64
    go b inp@(TestCfg _bacc sacc) =
      let actual = ExpectedBlock { addr = fromIntegral (R.absoluteAddress (R.basicBlockAddress b))
                                 , byteCount = R.concreteBlockSize R64.isa b
                                 , insnCount = length (R.basicBlockInstructions b)
                                 }
          blockStr = unlines (map (R.isaPrettyInstruction R64.isa) (R.basicBlockInstructions b))
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

testBlockRecovery :: (w ~ MM.ArchAddrWidth arch,
                      R.InstructionConstraints arch,
                      MBL.BinaryLoader arch binFmt,
                      E.ElfWidthConstraints w,
                      KnownNat w,
                      Typeable w,
                      R.ArchBits arch)
                  => C.HandleAllocator RealWorld
                  -> R.RenovateConfig arch binFmt TestConfig
                  -> E.Elf w
                  -> MBL.LoadedBinary arch binFmt
                  -> T.Assertion
testBlockRecovery hdlAlloc rc elf loadedBinary = do
  ((TestCfg status msgs), _) <- R.analyzeElf rc hdlAlloc elf loadedBinary
  T.assertBool (unlines ("Analysis Failed:" : msgs)) status

withELF :: FilePath -> (E.Elf 64 -> IO ()) -> IO ()
withELF fp k = do
  bytes <- B.readFile fp
  case E.parseElf bytes of
    E.ElfHeaderError off msg ->
      error ("Error parsing ELF header at offset " ++ show off ++ ": " ++ msg)
    E.Elf32Res [] _e32 -> error "ELF32 is unsupported in the test suite"
    E.Elf64Res [] e64 -> k e64
    E.Elf32Res errs _ -> error ("Errors while parsing ELF file: " ++ show errs)
    E.Elf64Res errs _ -> error ("Errors while parsing ELF file: " ++ show errs)

{-
trivialEntryPoints :: (MM.MemWidth w, Integral (E.ElfWordType w))
                   => MM.Memory w
                   -> E.Elf w
                   -> (MM.MemSegmentOff w, [MM.MemSegmentOff w])
trivialEntryPoints mem elf =
  let Just entryPoint = MM.resolveAbsoluteAddr mem (fromIntegral (E.elfEntry elf))
  in (entryPoint, [])
-}
