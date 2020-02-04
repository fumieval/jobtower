{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import RIO

import qualified Data.ByteString.Lazy as BL
import Options.Applicative
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath
import RIO.Process
import JobTower.Types
import qualified Network.HTTP.Simple as HC
import qualified Network.HTTP.Client as HC
import qualified RIO.Text as T
import qualified RIO.Map as M

data Opts = Opts
  { host :: String
  , verbose :: Bool
  , entrypoint :: String
  , arguments :: [String]
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "host" <> short 'h' <> value "localhost:1837" <> help "jobtower-server host")
  <*> switch (long "verbose" <> short 'v' <> help "verbose logging")
  <*> strArgument (metavar "CMD" <> help "command entrypoint")
  <*> many (strArgument (metavar "ARG"))

fetch :: Opts -> RIO LoggedProcessContext Job
fetch Opts{..} = do
  reqJob <- HC.parseUrlThrow $ "http://" <> host <> "/jobs/pop"
  job <- HC.responseBody <$> HC.httpJSON reqJob
    { HC.responseTimeout = HC.responseTimeoutNone }
  logInfo $ display $ jobId job

  return job

runJob :: Opts -> Job -> RIO LoggedProcessContext ()
runJob Opts{..} Job{..} = do
  let lp = display jobId <> ": "
  logInfo $ lp <> "Starting " <> displayShow jobArgs
  let prefix = ".jobtower-worker"
  createDirectoryIfMissing True prefix
  withFile (prefix </> "job-" <> T.unpack jobId <.> "log") WriteMode $ \logHandle ->
    withModifyEnvVars
    (M.insert "JOBTOWER_ID" jobId)
    $ proc entrypoint (arguments ++ map T.unpack jobArgs) $ \conf -> withProcessWait
    ( setStdin (byteStringInput $ BL.fromStrict $ encodeUtf8 jobInput)
    $ setStdout (useHandleOpen logHandle)
    $ setStderr (useHandleOpen logHandle) conf)
    $ \ph -> do
      code <- waitExitCode ph
      logInfo $ lp <> displayShow code

main :: IO ()
main = do
  opts <- execParser $ info parseOpts mempty
  logOpts <- logOptionsHandle stdout (verbose opts)
  pcxt <- mkDefaultProcessContext
  withLogFunc logOpts
    $ \lf -> runRIO (LoggedProcessContext pcxt lf)
    $ forever $ fetch opts >>= runJob opts
