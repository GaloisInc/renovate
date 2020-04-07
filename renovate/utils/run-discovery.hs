{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
import Control.Lens
import Control.Monad
import Data.ElfEdit
import Data.Foldable
import Data.Macaw.BinaryLoader
import Data.Macaw.Discovery
import Data.Macaw.Memory
import Data.Macaw.Memory.ElfLoader
import Data.Macaw.PPC hiding (entryPoints)
import Data.Macaw.X86
import Data.Maybe
import Data.Parameterized.Some
import Data.Proxy
import SemMC.Architecture.PPC64
import System.Environment
import System.Exit

import qualified Data.ByteString as BS
import qualified Data.Map as M

main :: IO ()
main = do
  [filename] <- getArgs
  bs <- BS.readFile filename
  elf <- case parseElf bs of
    Elf64Res warnings elf -> mapM_ print warnings >> return elf
    _ -> die "not a 64-bit ELF file"
  case elfMachine elf of
    EM_PPC64 -> do
      bin <- loadBinary @PPC defaultLoadOptions elf
      entries <- toList <$> entryPoints bin
      let pli = ppc64_linux_info bin
      showDiscoveryInfo $ cfgFromAddrs pli (memoryImage bin) M.empty entries []
    EM_X86_64 -> case resolveElfContents defaultLoadOptions elf of
      Left e -> fail (show e)
      Right (_, _, Nothing, _) -> fail "Couldn't work out entry point."
      Right (warn, mem, Just entryPoint, _) -> do
        mapM_ print warn
        showDiscoveryInfo $ cfgFromAddrs x86_64_linux_info mem M.empty [entryPoint] []
    _ -> fail "only X86 and PPC64 supported for now"

showDiscoveryInfo di =
  forM_ (M.toList (di ^. funInfo)) $ \(funAddr, Some dfi) -> do
    putStrLn $ "===== BEGIN FUNCTION " ++ show funAddr ++ " ====="
    forM_ (M.toList (dfi ^. parsedBlocks)) $ \(blockAddr, pb) -> do
      putStrLn $ "== begin block " ++ show blockAddr ++ " =="
      print $ pblockStmts pb
      print $ pblockTermStmt pb
      putStrLn ""
    putStrLn ""
    putStrLn ""
