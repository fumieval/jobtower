{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import RIO

import Control.Monad.Trans.Resource (runResourceT)
import Options.Applicative
import RIO.Directory
import RIO.Process
import JobTower.Types
import UnliftIO.Concurrent (forkIO, forkFinally)
import qualified Data.Conduit.Combinators as C
import qualified Data.Text.IO as T
import qualified Network.HTTP.Simple as HC
import qualified Network.HTTP.Client as HC
import qualified RIO.Text as T
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
  job@Job{..} <- HC.responseBody <$> HC.httpJSON reqJob
    { HC.responseTimeout = HC.responseTimeoutNone }
  logInfo $ display jobId

  forM_ jobFiles $ \name -> do
    let path = filesPrefix <> T.unpack name

    unlessM (doesFileExist path) $ do
      logInfo $ display jobId <> ": Fetching " <> display name
      reqFile <- HC.parseUrlThrow
        $ "http://" <> host <> "/files/" <> T.unpack name
      runResourceT $ HC.httpSink reqFile $ const $ C.sinkFile path
      getPermissions path >>= setPermissions path . setOwnerExecutable True
    
  return job

runJob :: Job -> RIO LoggedProcessContext ()
runJob Job{..} = do
  let lp = display jobId <> ": "
  logInfo $ lp <> "Starting " <> display jobCmd <> " " <> displayShow jobArgs
  proc (T.unpack jobCmd) (map T.unpack jobArgs) $ \conf -> withProcessWait
    ( setStdin createPipe
    $ setStdout createPipe
    $ setStderr createPipe conf)
    $ \ph -> do
      liftIO $ T.hPutStr (getStdin ph) jobInput
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
  createDirectoryIfMissing True filesPrefix
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
      forever $ threadDelay 1000