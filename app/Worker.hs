{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import RIO

import qualified Data.ByteString.Lazy as BL
import Options.Applicative
import RIO.Process
import JobTower.Types
import UnliftIO.Concurrent (forkIO, forkFinally)
import qualified Data.Text.IO as T
import qualified Network.HTTP.Simple as HC
import qualified Network.HTTP.Client as HC
import qualified RIO.Text as T
import qualified RIO.Map as M
import System.IO.Error (isEOFError)

data Opts = Opts
  { host :: String
  , jobs :: Int
  , verbose :: Bool
  }

parseOpts :: Parser Opts
parseOpts = Opts
  <$> strOption (long "host" <> short 'h' <> value "localhost:1837" <> help "jobtower-server host")
  <*> option auto (long "jobs" <> short 'j' <> value 1 <> help "Number of simultaneous jobs")
  <*> switch (long "verbose" <> short 'v' <> help "verbose logging")

fetch :: Opts -> RIO LoggedProcessContext Job
fetch Opts{..} = do
  reqJob <- HC.parseUrlThrow $ "http://" <> host <> "/jobs/pop"
  job <- HC.responseBody <$> HC.httpJSON reqJob
    { HC.responseTimeout = HC.responseTimeoutNone }
  logInfo $ display $ jobId job

  return job

runJob :: Job -> RIO LoggedProcessContext ()
runJob Job{..} = do
  let lp = display jobId <> ": "
  logInfo $ lp <> "Starting " <> display jobCmd <> " " <> displayShow jobArgs
  withModifyEnvVars
    (M.insert "JOBTOWER_ID" jobId)
    $ proc (T.unpack jobCmd) (map T.unpack jobArgs) $ \conf -> withProcessWait
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
    $ \lf -> runRIO (LoggedProcessContext pcxt lf) $ do
      replicateM_ (jobs opts) $ forkIO $ forever
        $ forever (fetch opts >>= runJob)
          `catch` \e -> do
            logError $ displayShow (e :: SomeException)
            logWarn "Resuming in 60s"
            threadDelay $ 60 * 1000 * 1000
      logInfo $ "Spawned " <> displayShow (jobs opts) <> " workers"
      forever $ threadDelay 1000
