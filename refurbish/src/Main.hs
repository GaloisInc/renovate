{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Lens ( (^.) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ElfEdit as E
import qualified Data.Foldable as F
import           Data.Functor.Const ( Const(..) )
import qualified Data.IntervalMap.Strict as IM
import           Data.Maybe ( fromMaybe )
import           Data.Monoid ((<>))
import qualified Data.Text.IO as T
import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Word ( Word64 )
import qualified Fmt as Fmt
import           Fmt ( (+|), (|+), (+||), (||+) )
import qualified Options.Applicative as O
import qualified System.Exit as IO
import qualified System.IO as IO

import qualified Data.Macaw.CFG as MM
import qualified Data.Macaw.BinaryLoader as MBL
import           Data.Macaw.BinaryLoader.X86 ()
import qualified Data.Parameterized.NatRepr as NR
import qualified Lang.Crucible.FunctionHandle as C

import qualified Renovate as R
import qualified Renovate.Arch.PPC as RP
import qualified Renovate.Arch.X86_64 as RX

import           Prelude hiding ((<>))

data Options = Options { oInput :: FilePath
                       , oOutput :: FilePath
                       , oBlockMappingFile :: Maybe FilePath
                       , oPrintOutputBlocks :: [Word64]
                       , oOutputBlockFile :: Maybe FilePath
                       , oPrintDiscoveredBlocks :: [Word64]
                       , oDiscoveredBlockFile :: Maybe FilePath
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
                        <*> O.optional (O.strOption ( O.long "print-block-mapping"
                                                      <> O.help "Print the mapping from original block addresses to rewritten block addresses (pass - to print to stdout)"
                                                    )
                                       )
                        <*> O.many (O.option O.auto ( O.long "print-output-block"
                                                      <> O.metavar "ADDRESS"
                                                      <> O.help "Print the contents of the (output) block containing the given address (can be specified multiple times)"
                                                      )
                                   )
                        <*> O.optional (O.strOption ( O.long "output-block-file"
                                                   <> O.metavar "FILE"
                                                   <> O.help "The file to write output blocks (specified by --print-output-block) to (default: stdout)"
                                                    )
                                       )
                        <*> O.many (O.option O.auto ( O.long "print-discovered-block"
                                                    <> O.metavar "ADDRESS"
                                                    <> O.help "Print the contents of the discovered block containing the given address (can be specified multiple times)"
                                                    )
                                   )
                        <*> O.optional (O.strOption ( O.long "discovered-block-file"
                                                    <> O.metavar "FILE"
                                                    <> O.help "The file to write discovered blocks (specified by --print-discovered-block) to (default: stdout)"
                                                    )
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
  hdlAlloc <- C.newHandleAllocator
  case E.parseElf bytes of
    E.ElfHeaderError _ err -> do
      putStrLn err
      IO.exitFailure
    E.Elf32Res errs e32 -> do
      case errs of
        [] -> return ()
        _ -> print errs
      R.withElfConfig (E.Elf32 e32) configs $ \rc e loadedBinary -> do
        let rc' = rc { R.rcUpdateSymbolTable = True }
        (e', _, ri) <- R.rewriteElf rc' hdlAlloc e loadedBinary R.Parallel
        printInfo o ri
        LBS.writeFile (oOutput o) (E.renderElf e')
    E.Elf64Res errs e64 -> do
      case errs of
        [] -> return ()
        _ -> print errs
      R.withElfConfig (E.Elf64 e64) configs $ \rc e loadedBinary -> do
        let rc' = rc { R.rcUpdateSymbolTable = True }
        (e', _, ri) <- R.rewriteElf rc' hdlAlloc e loadedBinary R.Parallel
        printInfo o ri
        LBS.writeFile (oOutput o) (E.renderElf e')

-- | Print out all of the information requested by the user via the command line 'Options'
--
-- Factored out so that we can call it once for both 32 and 64 bits
printInfo :: (MM.MemWidth (MM.ArchAddrWidth arch))
          => Options
          -> R.RewriterInfo arch
          -> IO ()
printInfo o ri = do
  withHandleWhen (oBlockMappingFile o) (printBlockMapping (ri ^. R.riBlockMapping))
  F.forM_ (ri ^. R.riOutputBlocks) (printRequestedBlocks (oPrintOutputBlocks o) (oOutputBlockFile o))
  F.forM_ (ri ^. R.riRecoveredBlocks) (printRequestedBlocks (oPrintDiscoveredBlocks o) (oDiscoveredBlockFile o))

printRequestedBlocks :: [Word64] -> Maybe FilePath -> R.SomeBlocks -> IO ()
printRequestedBlocks reqs mOutFile (R.SomeBlocks isa blocks) = do
  let idx = indexBlocks isa blocks
  let cbs = [ b
            | addrWord <- reqs
            , b <- IM.elems (IM.containing idx (R.concreteFromAbsolute (fromIntegral addrWord)))
            ]
  withHandleWhen (Just (fromMaybe "-" mOutFile)) $ \h ->
    F.forM_ cbs (printOutputBlock h isa)

-- | Index a collection of blocks into an 'IM.IntervalMap' by the range of addresses they contain
indexBlocks :: (MM.MemWidth (MM.ArchAddrWidth arch))
            => R.ISA arch
            -> [R.ConcreteBlock arch]
            -> IM.IntervalMap (R.ConcreteAddress arch) (R.ConcreteBlock arch)
indexBlocks isa = foldr indexBlock IM.empty
  where
    indexBlock cb im =
      let sz = R.concreteBlockSize isa cb
          addr0 = R.basicBlockAddress cb
          i = IM.IntervalCO addr0 (addr0 `R.addressAddOffset` fromIntegral sz)
      in IM.insert i cb im

-- | If provided a 'FilePath', open that file (writable) and call the
-- continuation with that handle.
--
-- Otherwise, do nothing.
withHandleWhen :: Maybe FilePath -> (IO.Handle -> IO ()) -> IO ()
withHandleWhen mf k =
  case mf of
    Nothing -> return ()
    Just fn
      | fn == "-" -> k IO.stdout
      | otherwise -> IO.withFile fn IO.WriteMode k

-- | Trivial no-op analysis
analyze :: R.AnalyzeEnv arch -> MBL.LoadedBinary arch binFmt -> IO (Const () arch)
analyze _ _ = return (Const ())

-- | Trivial rewriting that changes no instructions, but causes all eligible blocks to be relocated
rewrite :: Const () arch
        -> MBL.LoadedBinary arch binFmt
        -> R.SymbolicBlock arch
        -> R.RewriteM arch (Maybe [R.TaggedInstruction arch (R.InstructionAnnotation arch)])
rewrite _ _ b = return (Just (R.basicBlockInstructions b))

-- | Format a 'R.ConcreteBlock' to the given 'IO.Handle'
printOutputBlock :: (R.InstructionConstraints arch)
                 => IO.Handle
                 -> R.ISA arch
                 -> R.ConcreteBlock arch
                 -> IO ()
printOutputBlock h isa cb = do
  T.hPutStrLn h (Fmt.fmt ("bb:" +|| PD.pretty (R.basicBlockAddress cb) ||+ ""))
  F.forM_ (R.instructionAddresses isa cb) $ \(i, addr) -> do
    T.hPutStrLn h (Fmt.fmt (PD.pretty addr ||+ ": " +| R.isaPrettyInstruction isa i |+ ""))
  T.hPutStrLn h ""
  return ()

-- | Print out a mapping of original block addresses to rewritten block addresses to the given 'IO.Handle'
printBlockMapping :: (MM.MemWidth (MM.ArchAddrWidth arch))
                  => [(R.ConcreteAddress arch, R.ConcreteAddress arch)]
                  -> IO.Handle
                  -> IO ()
printBlockMapping bm h = do
  F.forM_ bm $ \(origAddr, newAddr) -> do
    T.hPutStrLn h (Fmt.fmt (PD.pretty origAddr ||+ " -> " +|| PD.pretty newAddr ||+ ""))
