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
  runInContainer :: [(FilePath, FilePath)]
                 -> [String]
                 -> IO (E.ExitCode, String, String)
  }

-- | An implementation of the 'Runner' newtype.
runner :: [(FilePath, FilePath)] -> [String] -> IO (E.ExitCode, String, String)
runner mapping cmdline =
  P.readProcessWithExitCode "docker" dargs ""
  where
    argMap = [["-v", src ++ ":" ++ dst] | (src, dst) <- mapping]
    dargs = concat [ ["run", "--init", "--rm"]
                   , concat argMap
                   , [qemuRunnerName]
                   , cmdline
                   ]
