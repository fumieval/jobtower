{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import RIO

import qualified Data.ByteString.Lazy as BL
import Options.Applicative
import RIO.Process
import JobTower.Types
import UnliftIO.Concurrent (forkFinally)
import qualified Data.Text.IO as T
import qualified Network.HTTP.Simple as HC
import qualified Network.HTTP.Client as HC
import qualified RIO.Text as T
import qualified RIO.Map as M
import System.IO.Error (isEOFError)

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
  withModifyEnvVars
    (M.insert "JOBTOWER_ID" jobId)
    $ proc entrypoint (arguments ++ map T.unpack jobArgs) $ \conf -> withProcessWait
    ( setStdin (byteStringInput $ BL.fromStrict $ encodeUtf8 jobInput)
    $ setStdout createPipe
    $ setStderr createPipe conf)
    $ \ph -> do
      let finish (Right _) = pure ()
          finish (Left e) | Just e' <- fromException e, isEOFError e' = pure ()
          finish (Left e) = logError $ displayShow e
          pipe p f = flip forkFinally finish $ forever $ do
            ln <- liftIO $ T.hGetLine (p ph)
            logInfo $ lp <> f <> display ln
      _ <- pipe getStdout "STDOUT: "
      _ <- pipe getStderr "STDERR: "
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
