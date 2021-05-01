import           Control.Monad
import           Data.Functor.Contravariant ( (>$<) )
import qualified Data.Text.IO as TIO
import qualified Lumberjack as LJ
import qualified System.Directory as SD
import qualified System.Environment as SE
import qualified System.Exit as SE
import           System.FilePath ( (</>) )
import qualified System.FilePath as SF
import qualified System.IO as IO
import           Text.Printf

import qualified Refurbish.Docker as RD

getRunner :: IO RD.Runner
getRunner = do
  eRunner <- RD.initializeQemuRunner
  case eRunner of
    Left (ec, out, err) -> do
      let msg = unlines
            [ "Failed to initialize qemu-runner container: " ++ show ec
            , "(Is Docker installed? Are you in the docker group?)"
            , out
            , err
            ]
      SE.die msg
    Right runner -> return runner

runExe :: RD.Runner -> FilePath -> [String] -> IO b
runExe runner exe args = do
  let logger = LJ.cvtLogMessageToANSITermText >$< LJ.LogAction (TIO.hPutStrLn IO.stderr)
  exeExists <- SD.doesFileExist exe
  when (not exeExists) $
    usage $ Just (printf "No such executable: %s" exe)
  localPath <- SD.canonicalizePath exe
  -- The name of the executable mapped into the container
  let containerPath = "/tmp" </> SF.takeFileName exe
  (origExit, origOut, origErr) <-
    RD.runInContainer runner logger [(localPath, containerPath)]
      (containerPath : args)
  IO.hPutStr IO.stdout origOut
  IO.hPutStr IO.stderr origErr
  SE.exitWith origExit

usage :: Maybe String -> IO a
usage mreason = do
  progName <- SE.getProgName
  let prefix = case mreason of
        Nothing -> []
        Just reason -> [reason, ""]
  let msg = unlines $
        prefix ++
        [ printf "usage: %s EXE ARGS" progName
        , ""
        , "Run 'EXE ARGS' via QEMU in a Docker container."
        , "Prints the stdout and stderr and returns the exit code,"
        , "as if run locally. Note that 'EXE' is an executable on"
        , "the local host and will copied into the Docker container." ]
  SE.die msg

main :: IO ()
main = do
  args <- SE.getArgs
  when (length args == 0 ||
        args !! 0 `elem` ["-h", "--help"]) $
    usage Nothing
  runner <- getRunner
  let exe : exeArgs = args
  runExe runner exe exeArgs
