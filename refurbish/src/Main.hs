{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ElfEdit as E
import           Data.Functor.Const ( Const(..) )
import qualified Options.Applicative as O
import qualified System.Exit as IO

import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Parameterized.NatRepr as NR

import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP
import qualified Renovate.Arch.X86_64 as RX

data Options = Options { oInput :: FilePath
                       , oOutput :: FilePath
                       }

optionsParser :: O.Parser Options
optionsParser = Options <$> O.strArgument (  O.metavar "FILE"
                                          <> O.help "The ELF file to rewrite"
                                          )
                        <*> O.strOption ( O.long "output"
                                        <> O.short 'o'
                                        <> O.metavar "FILE"
                                        <> O.help "The output file to write"
                                        )

main :: IO ()
main = O.execParser optParser >>= mainWithOptions
  where
    optParser = O.info ( optionsParser O.<**> O.helper )
                       ( O.fullDesc
                       <> O.progDesc "A tool to apply a trivial rewriting to PowerPC and X86_64 binaries"
                       <> O.header "refurbish - A trivial binary rewriter"
                       )

mainWithOptions :: Options -> IO ()
mainWithOptions o = do
  bytes <- BS.readFile (oInput o)
  let configs :: [(R.Architecture, R.SomeConfig R.TrivialConfigConstraint (Const ()))]
      configs = [ (R.PPC32, R.SomeConfig (NR.knownNat @32) MBL.Elf32Repr (RP.config32 analyze rewrite))
                , (R.PPC64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RP.config64 analyze rewrite))
                , (R.X86_64, R.SomeConfig (NR.knownNat @64) MBL.Elf64Repr (RX.config analyze rewrite))
                ]
  case E.parseElf bytes of
    E.ElfHeaderError _ err -> do
      putStrLn err
      IO.exitFailure
    E.Elf32Res errs e32 -> do
      case errs of
        [] -> return ()
        _ -> print errs
      R.withElfConfig (E.Elf32 e32) configs $ \rc e loadedBinary -> do
        (e', _, _) <- R.rewriteElf rc e loadedBinary R.Parallel
        LBS.writeFile (oOutput o) (E.renderElf e')
    E.Elf64Res errs e64 -> do
      case errs of
        [] -> return ()
        _ -> print errs
      R.withElfConfig (E.Elf64 e64) configs $ \rc e loadedBinary -> do
        (e', _, _) <- R.rewriteElf rc e loadedBinary R.Parallel
        LBS.writeFile (oOutput o) (E.renderElf e')

analyze :: R.AnalyzeEnv arch -> MBL.LoadedBinary arch binFmt -> IO (Const () arch)
analyze _ _ = return (Const ())

rewrite :: Const () arch
        -> MBL.LoadedBinary arch binFmt
        -> R.SymbolicBlock arch
        -> R.RewriteM arch (Maybe [R.TaggedInstruction arch (R.InstructionAnnotation arch)])
rewrite _ _ b = return (Just (R.basicBlockInstructions b))
