{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Tests for the x64 ABI
module X64 ( x64Tests ) where

import           GHC.TypeLits ( KnownNat )
import           Data.Typeable ( Typeable )

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Word ( Word64 )
import System.FilePath ( (<.>), replaceExtension )
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Text.Read ( readMaybe )

import qualified Data.Parameterized.NatRepr as NR
import qualified Data.Macaw.CFG as MM

import qualified Renovate as R
import qualified Renovate.Arch.X86_64 as R64

import qualified Data.ElfEdit as E

x64Tests :: [FilePath] -> T.TestTree
x64Tests asmFiles = T.testGroup "x64 ABI tests" (map mkTest asmFiles)

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

class (KnownNat (MM.ArchAddrWidth arch)) => TestConstraint arch b where
instance TestConstraint R64.X86_64 b

mkTest :: FilePath -> T.TestTree
mkTest fp = T.testCase fp $ withELF elfFilename testRewrite
  where
    testRewrite :: E.Elf 64 -> IO ()
    testRewrite elf = do
      Just expected <- readMaybe <$> readFile (fp <.> "expected")
      let cfg :: [(R.Architecture, R.SomeConfig TestConstraint (Bool, [String]))]
          cfg = [(R.X86_64, R.SomeConfig NR.knownNat (R64.config (analysis expected) R.identity))]
      R.withElfConfig (E.Elf64 elf) cfg testBlockRecovery

    elfFilename = replaceExtension fp "exe"

analysis :: ExpectedResult -> R.ISA R64.X86_64 -> MM.Memory 64 -> R.BlockInfo R64.X86_64 -> (Bool,[String])
analysis expected isa _mem blocks =
  foldr go (True,[]) (R.biBlocks blocks)
  where
    go :: R.ConcreteBlock R64.X86_64 -> (Bool,[String]) -> (Bool,[String])
    go b (bacc,sacc) =
      let actual = ExpectedBlock { addr = fromIntegral (R.absoluteAddress (R.basicBlockAddress b))
                                 , byteCount = R.concreteBlockSize isa b
                                 , insnCount = length (R.basicBlockInstructions b)
                                 }
          blockStr = unlines (map (R.isaPrettyInstruction isa) (R.basicBlockInstructions b))
      in case M.lookup (addr actual) expectedMap of
        Nothing
          | S.member (addr actual) ignoreSet -> (bacc,sacc)
          | otherwise -> (False,("Unexpected block: " ++ show actual ++ " with instructions\n" ++ blockStr):sacc)
        Just eb -> case eb == actual of
                   True  -> (bacc, sacc)
                   False -> (False, ("Block mismatch:\n" ++ blockStr):sacc)
    expectedMap = M.fromList [ (addr eb, eb) | eb <- expectedBlocks expected ]
    ignoreSet = S.fromList (ignoreBlocks expected)

testBlockRecovery :: (w ~ MM.ArchAddrWidth arch,
                      R.InstructionConstraints arch,
                      E.ElfWidthConstraints w,
                      KnownNat w,
                      Typeable w,
                      R.ArchBits arch)
                  => R.RenovateConfig arch (Bool,[String])
                  -> E.Elf w
                  -> MM.Memory w
                  -> T.Assertion
testBlockRecovery rc elf mem = do
  ((status, msgs), _) <- R.analyzeElf rc elf mem
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
