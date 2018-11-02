{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Main ( main ) where

import           Control.Applicative ( (<|>) )
import qualified Control.Exception as X
import           Control.Lens ( (^.) )
import           Control.Monad ( when )
import           Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ElfEdit as E
import qualified Data.Foldable as F
import           Data.Functor.Const ( Const(..) )
import qualified Data.IntervalMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe ( fromMaybe )
import           Data.Monoid ((<>))
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Text.IO as T
import qualified Data.Text.Prettyprint.Doc as PD
import           Data.Word ( Word64 )
import qualified Fmt as Fmt
import           Fmt ( (+|), (|+), (+||), (||+) )
import qualified Language.Scheme.Core as LSC
import qualified Language.Scheme.Parser as LSP
import qualified Language.Scheme.Types as LST
import qualified Options.Applicative as O
import qualified System.Console.Haskeline as H
import qualified System.Exit as IO
import qualified System.IO as IO
import           Text.Read ( readMaybe )

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
                       , oRunREPL :: Bool
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
                        <*> O.switch ( O.long "repl"
                                     <> O.help "Start a REPL after processing all other options"
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
        when (oRunREPL o) (runREPL ri)
    E.Elf64Res errs e64 -> do
      case errs of
        [] -> return ()
        _ -> print errs
      R.withElfConfig (E.Elf64 e64) configs $ \rc e loadedBinary -> do
        let rc' = rc { R.rcUpdateSymbolTable = True }
        (e', _, ri) <- R.rewriteElf rc' hdlAlloc e loadedBinary R.Parallel
        printInfo o ri
        LBS.writeFile (oOutput o) (E.renderElf e')
        when (oRunREPL o) (runREPL ri)

data REPLInfo =
  REPLInfo { rewriterInfo :: Some R.RewriterInfo
           , discoveredIndex :: Maybe SomeBlockIndex
           , outputIndex :: Maybe SomeBlockIndex
           , blockMapping :: Some BlockMapping
           }

newtype BlockMapping arch =
  BlockMapping (M.Map (R.ConcreteAddress arch) (R.ConcreteAddress arch))

data SomeBlockIndex =
  forall arch . (MM.MemWidth (MM.ArchAddrWidth arch), R.InstructionConstraints arch) =>
  SomeBlockIndex { _blockIndex :: IM.IntervalMap (R.ConcreteAddress arch) (R.ConcreteBlock arch)
                 , _blockISA :: R.ISA arch
                 }

runREPL :: R.RewriterInfo arch -> IO ()
runREPL ri = H.runInputT settings' (repl hdlrs)
  where
    ws = [' ', '\t']
    settings :: H.Settings IO
    settings = H.defaultSettings
    settings' = settings { H.complete = H.completeWordWithPrev Nothing ws (completeCommands hdlrs) }
    hdlrs = commandHandlers info
    info = REPLInfo { rewriterInfo = Some ri
                    , discoveredIndex =
                      case ri ^. R.riRecoveredBlocks of
                        Nothing -> Nothing
                        Just (R.SomeBlocks isa bs) -> Just (SomeBlockIndex (indexBlocks isa bs) isa)
                    , outputIndex =
                      case ri ^. R.riOutputBlocks of
                        Nothing -> Nothing
                        Just (R.SomeBlocks isa bs) -> Just (SomeBlockIndex (indexBlocks isa bs) isa)
                    , blockMapping = Some (BlockMapping (M.fromList (ri ^. R.riBlockMapping)))
                    }

data CommandHandler =
  CommandHandler { commandHandler :: String -> [String] -> H.InputT IO ()
                 , commandDesc :: String
                 , commandName :: String
                 }

newtype CommandHandlers = CommandHandlers { unCH :: M.Map String CommandHandler }

completeCommands :: CommandHandlers -> String -> String -> IO [H.Completion]
completeCommands ch prev word
  | prev == " lave" = H.listFiles word
  | prev == " pleh" = return [ H.simpleCompletion cmd
                            | cmd <- cmds
                            , word `L.isPrefixOf` cmd
                            ]
  | prev /= "" = return [] -- We don't complete words that aren't commands (except for eval)
  | otherwise = return [ H.simpleCompletion cmd
                       | cmd <- cmds
                       , word `L.isPrefixOf` cmd
                       ]
  where
    cmds = quitCommands ++ M.keys (unCH ch)

quitCommands :: [String]
quitCommands = ["quit", "exit"]

repl :: CommandHandlers -> H.InputT IO ()
repl hdlrs = do
  ml <- H.getInputLine "> "
  case ml of
    Nothing -> return ()
    Just l
      | l `elem` quitCommands -> return ()
      | otherwise -> processLine hdlrs l >> repl hdlrs

processLine :: CommandHandlers -> String -> H.InputT IO ()
processLine (CommandHandlers hdlrs) l =
  case words l of
    cmd : args
      | Just h <- M.lookup cmd hdlrs -> commandHandler h cmd args
      | otherwise -> H.outputStrLn ("Invalid command: " +| cmd |+ "")
    [] -> return ()

commandHandlers :: REPLInfo -> CommandHandlers
commandHandlers ri = CommandHandlers m
  where
    m = M.fromList [ (commandName c, c) | c <- cs ]
    cs = [ replPrintDiscoveredBlock ri
         , replPrintOutputBlock ri
         , replAddressInfo ri
         , replEval ri
         , replHelp cs
         ]

replHelp :: [CommandHandler] -> CommandHandler
replHelp cs =
  CommandHandler { commandName = "help"
                 , commandDesc = "Display help"
                 , commandHandler = hdlr
                 }
  where
    m = M.fromList [ (commandName c, c) | c <- cs ]
    hdlr helpCmd args =
      case args of
        [] -> F.forM_ cs $ \c -> do
          H.outputStrLn ("" +| commandName c |+ ": " +| commandDesc c |+ "")
        [c] ->
          case M.lookup c m of
            Nothing -> H.outputStrLn ("Invalid command: " +| c |+ "")
            Just cmd -> H.outputStrLn (commandDesc cmd)
        _ -> invalidArguments helpCmd

replPrintDiscoveredBlock :: REPLInfo -> CommandHandler
replPrintDiscoveredBlock ri =
  CommandHandler { commandName = "print-discovered-block"
                 , commandDesc = "Print the (unmodified) block containing the given address from the original text section"
                 , commandHandler = hdlr
                 }
  where
    hdlr cmd args =
      case args of
        [mAddr]
          | Just (addr :: Word64) <- readMaybe mAddr ->
            case discoveredIndex ri of
              Nothing -> H.outputStrLn "No discovered blocks"
              Just (SomeBlockIndex idx isa) -> do
                let caddr = R.concreteFromAbsolute (fromIntegral addr)
                F.forM_ (IM.elems (IM.containing idx caddr)) $ printOutputBlock H.outputStr isa
        _ -> invalidArguments cmd

replPrintOutputBlock :: REPLInfo -> CommandHandler
replPrintOutputBlock ri =
  CommandHandler { commandName = "print-output-block"
                 , commandDesc = "Print the block containing the given address from the rewritten code"
                 , commandHandler = hdlr
                 }
  where
    hdlr cmd args =
      case args of
        [mAddr]
          | Just (addr :: Word64) <- readMaybe mAddr ->
            case outputIndex ri of
              Nothing -> H.outputStrLn "No output blocks"
              Just (SomeBlockIndex idx isa) -> do
                let caddr = R.concreteFromAbsolute (fromIntegral addr)
                F.forM_ (IM.elems (IM.containing idx caddr)) $ printOutputBlock H.outputStr isa
        _ -> invalidArguments cmd

replAddressInfo :: REPLInfo -> CommandHandler
replAddressInfo ri =
  CommandHandler { commandName = "address-info"
                 , commandDesc = "Print information about the block containing the given address"
                 , commandHandler = hdlr
                 }
  where
    hdlr cmd args =
      case args of
        [mAddr]
          | Just (addr :: Word64) <- readMaybe mAddr ->
            case tryOriginalAddress ri addr <|> tryOutputAddress ri addr of
              Nothing -> H.outputStrLn ("Address is not a code address: " +| Fmt.hexF addr |+ "")
              Just h -> h
        _ -> invalidArguments cmd

tryOriginalAddress :: REPLInfo -> Word64 -> Maybe (H.InputT IO ())
tryOriginalAddress ri addr = do
  bi <- discoveredIndex ri
  case bi of
    SomeBlockIndex idx isa -> do
      let caddr = R.concreteFromAbsolute (fromIntegral addr)
      case IM.elems (IM.containing idx caddr) of
        [] -> Nothing
        bs -> Just (replOriginalBlockInfo ri isa addr caddr bs)

tryOutputAddress :: REPLInfo -> Word64 -> Maybe (H.InputT IO ())
tryOutputAddress ri addr = do
  bi <- outputIndex ri
  case bi of
    SomeBlockIndex idx isa -> do
      let caddr = R.concreteFromAbsolute (fromIntegral addr)
      case IM.elems (IM.containing idx caddr) of
        [] -> Nothing
        bs -> Just (replOutputBlockInfo ri isa addr caddr bs)

plurality :: [a] -> String
plurality ls =
  case ls of
    [_] -> ""
    _ -> "s"

replOriginalBlockInfo :: (R.InstructionConstraints arch)
                      => REPLInfo
                      -> R.ISA arch
                      -> Word64
                      -> R.ConcreteAddress arch
                      -> [R.ConcreteBlock arch]
                      -> H.InputT IO ()
replOriginalBlockInfo ri isa waddr addr bs = do
  H.outputStrLn ("The instruction at " +|| PD.pretty addr ||+ " was in the original text section in block" +| plurality bs |+ ":")
  F.forM_ bs (printOutputBlock H.outputStr isa)
{-
  case outputIndex ri of
    Nothing -> H.outputStrLn "There are no output blocks"
    Just (SomeBlockIndex idx isa') -> do
      let Some (BlockMapping bm) = blockMapping ri
      let caddr' = R.concreteFromAbsolute (fromIntegral waddr)
      case M.lookup caddr' bm of
        Nothing ->
          H.outputStrLn "It has not been changed by the rewriter"
        Just outputBlockAddr -> do
          let outBlockAddr' = R.concreteFromAbsolute (fromIntegral (R.absoluteAddress outputBlockAddr))
          let obs = IM.elems (IM.containing idx outBlockAddr')
          H.outputStrLn ("It occurs in the following output block" +| plurality obs |+ "")
          F.forM_ obs (printOutputBlock H.outputStr isa')
-}

replOutputBlockInfo :: (R.InstructionConstraints arch)
                    => REPLInfo
                    -> R.ISA arch
                    -> Word64
                    -> R.ConcreteAddress arch
                    -> [R.ConcreteBlock arch]
                    -> H.InputT IO ()
replOutputBlockInfo ri isa waddr addr bs = do
  H.outputStrLn ("The instruction at " +|| PD.pretty addr ||+ " is in a block added by the rewriter")
  F.forM_ bs (printOutputBlock H.outputStr isa)

replEval :: REPLInfo -> CommandHandler
replEval _ri =
  CommandHandler { commandName = "eval"
                 , commandDesc = "Evaluate a scheme program (with access to the rewriter metrics)"
                 , commandHandler = hdlr
                 }
  where
    hdlr cmd args =
      case args of
        [inputFile] -> do
          econtents <- liftIO (X.try (readFile inputFile))
          case econtents of
            Left (err :: X.IOException) -> H.outputStrLn ("Error reading file " +| inputFile |+ ": " +|| err ||+ "")
            Right scm -> do
              env <- liftIO $ LSC.r7rsEnv
              _ <- liftIO $ LSC.runIOThrowsREPL $ do
                exprs <- LST.liftThrows $ LSP.readExprList scm
                vals <- mapM (LSC.evalLisp env) exprs
                return (show vals)
              return ()
        _ -> invalidArguments cmd

invalidArguments :: String -> H.InputT IO ()
invalidArguments cmd =
  H.outputStrLn ("Invalid arguments for command " +| cmd |+ "")

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
            , let caddr = R.concreteFromAbsolute (fromIntegral addrWord)
            , b <- IM.elems (IM.containing idx caddr)
            ]
  withHandleWhen (Just (fromMaybe "-" mOutFile)) $ \h ->
    F.forM_ cbs (printOutputBlock (IO.hPutStr h) isa)

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
printOutputBlock :: (Monad m, R.InstructionConstraints arch)
                 => (String -> m ())
                 -> R.ISA arch
                 -> R.ConcreteBlock arch
                 -> m ()
printOutputBlock put isa cb = do
  put (Fmt.fmtLn ("bb:" +|| PD.pretty (R.basicBlockAddress cb) ||+ ""))
  F.forM_ (R.instructionAddresses isa cb) $ \(i, addr) -> do
    put (Fmt.fmtLn (PD.pretty addr ||+ ": " +| R.isaPrettyInstruction isa i |+ ""))
  put "\n"
  return ()

-- | Print out a mapping of original block addresses to rewritten block addresses to the given 'IO.Handle'
printBlockMapping :: (MM.MemWidth (MM.ArchAddrWidth arch))
                  => [(R.ConcreteAddress arch, R.ConcreteAddress arch)]
                  -> IO.Handle
                  -> IO ()
printBlockMapping bm h = do
  F.forM_ bm $ \(origAddr, newAddr) -> do
    T.hPutStrLn h (Fmt.fmt (PD.pretty origAddr ||+ " -> " +|| PD.pretty newAddr ||+ ""))
