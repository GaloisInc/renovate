{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
import Control.Lens
import Control.Monad
import Data.ElfEdit as EE
import Data.Foldable
import Data.Macaw.BinaryLoader
import Data.Macaw.BinaryLoader.PPC ()
import Data.Macaw.BinaryLoader.X86 ()
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
  case EE.decodeElfHeaderInfo bs of
    Left (off, err) -> die ("Error decoding ELF file at " ++ show off ++ ": " ++ err)
    Right (EE.SomeElf e) ->
      case EE.headerClass (EE.header e) of
        EE.ELFCLASS32 -> die "32 bit binaries not supported"
        EE.ELFCLASS64 ->
          case EE.headerMachine (EE.header e) of
            EM_PPC64 -> do
              bin <- loadBinary @PPC defaultLoadOptions e
              entries <- toList <$> entryPoints bin
              let pli = ppc64_linux_info bin
              showDiscoveryInfo $ cfgFromAddrs pli (memoryImage bin) M.empty entries []
            EM_X86_64 -> do
              bin <- loadBinary @X86_64 defaultLoadOptions e
              entries <- toList <$> entryPoints bin
              showDiscoveryInfo (cfgFromAddrs x86_64_linux_info (memoryImage bin) M.empty entries [])
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
