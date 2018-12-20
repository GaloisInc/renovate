-- | Utilities for running executables (via qemu) in a docker container
module Refurbish.Docker (
  Runner,
  runInContainer,
  initializeQemuRunner
  ) where

import qualified System.Exit as E
import qualified System.Process as P

qemuRunnerName :: String
qemuRunnerName = "refurbish-qemu-runner"

dockerFile :: String
dockerFile =
  unlines [ "FROM ubuntu:18.04"
          , "RUN apt update"
          , "RUN apt install -y qemu-user"
          , "WORKDIR /tmp"
          ]

initializeQemuRunner :: IO (Either (Int, String, String) Runner)
initializeQemuRunner = do
  (ec, out, err) <- P.readProcessWithExitCode "docker" ["build", "-t", qemuRunnerName, "-"] dockerFile
  case ec of
    E.ExitSuccess -> do return (Right (Runner runner))
    E.ExitFailure rv -> return (Left (rv, out, err))

-- | * A mapping of local filenames to in-container filenames - all must be absolute
--   * Command line to run
newtype Runner = Runner { runInContainer :: [(FilePath, FilePath)] -> [String] -> IO (E.ExitCode, String, String) }

runner :: [(FilePath, FilePath)] -> [String] -> IO (E.ExitCode, String, String)
runner mapping args =
  P.readProcessWithExitCode "docker" dargs ""
  where
    argMap = [["-v", src ++ ":" ++ dst] | (src, dst) <- mapping]
    dargs = concat [ ["run", "--rm"]
                   , concat argMap
                   , [qemuRunnerName]
                   , args
                   ]
