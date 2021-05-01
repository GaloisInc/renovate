{-# LANGUAGE OverloadedStrings #-}
-- | Utilities for running executables (via qemu) in a docker container
module Refurbish.Docker (
  Runner,
  runInContainer,
  initializeQemuRunner
  ) where

import qualified Data.List as L
import qualified Data.Text as T
import           Lumberjack ( (|#) )
import qualified Lumberjack as LJ
import qualified Refurbish.QEMU as Q
import qualified System.Exit as E
import           System.FilePath ( takeFileName )
import qualified System.Process as P


qemuRunnerName :: String
qemuRunnerName = "refurbish-qemu-runner"

dockerFile :: String
dockerFile =
  unlines [ "FROM ubuntu:20.04"
          , "RUN apt update"
          , "RUN apt install -y qemu-user"
          , "WORKDIR /tmp"
          ]

initializeQemuRunner :: IO (Either (Int, String, String) Runner)
initializeQemuRunner = do
  -- WARNING: if the output is not a valid 'String' in the current
  -- encoding then the 'hGetcontents' call in the implementation
  -- 'P.readProcessWithExitCode' here can fail with
  --
  -- > hGetContents: invalid argument (invalid byte sequence)
  --
  -- Ran into this when testing @gzip@ with compressed file going to
  -- stdout.
  (ec, out, err) <- P.readProcessWithExitCode "docker" ["build", "-t", qemuRunnerName, "-"] dockerFile
  case ec of
    E.ExitSuccess -> do return (Right (Runner runner))
    E.ExitFailure rv -> return (Left (rv, out, err))

-- | Run a command in a docker container.
--
-- The arguments are
--
-- * A mapping of local filenames to in-container filenames - all must be absolute
-- * Command line to run, e.g. @["/bin/ls", "-l", ".."]@.
--
-- Returns the exit code, stdout, and stderr.
newtype Runner = Runner {
  runInContainer :: LJ.LogAction IO LJ.LogMessage
                 -> [(FilePath, FilePath)]
                 -> [String]
                 -> IO (E.ExitCode, String, String)
  }

-- | An implementation of the 'Runner' newtype.
runner :: LJ.LogAction IO LJ.LogMessage
       -> [(FilePath, FilePath)] -> [String] -> IO (E.ExitCode, String, String)
runner logger mapping cmdline = do
  q <- Q.qemulator localExe
  let da = dargs q
  LJ.writeLog logger |# "running: docker " <> (T.pack $ L.intercalate " " da)
  r <- P.readProcessWithExitCode "docker" da ""
  LJ.writeLog logger |# "  --> " <> LJ.tshow r
  return r
  where
    cmdName = takeFileName $ head cmdline
    localExe = case filter ((cmdName ==) . takeFileName . snd) mapping of
                 [] -> error "Cannot determine file being executed"
                 (a:[]) -> fst a
                 _ -> error "Cannot uniquely determine file being executed"
    argMap = [["-v", src ++ ":" ++ dst ++ ":Z"] | (src, dst) <- mapping]
    dargs q = concat [ ["run", "--init", "--rm", "--privileged"]
                     , concat argMap
                     , [qemuRunnerName]
                     , [q]
                     , cmdline
                     ]
