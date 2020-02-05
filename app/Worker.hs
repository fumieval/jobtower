{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import RIO

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Options.Applicative as O
import RIO.Directory (createDirectoryIfMissing)
import RIO.FilePath
import RIO.Process
import JobTower.Types
import qualified Network.HTTP.Simple as HCS
import qualified Network.HTTP.Client as HC
import qualified RIO.Text as T
import qualified RIO.Map as M
import Data.Time (getCurrentTime)
import Network.HTTP.Types (http20)

data Opts = Opts
  { host :: String
  , verbose :: Bool
  , entrypoint :: String
  , arguments :: [String]
  }

parseOpts :: O.Parser Opts
parseOpts = Opts
  <$> O.strOption (O.long "host" <> O.short 'h' <> O.value "localhost:1837" <> O.help "jobtower-server host")
  <*> O.switch (O.long "verbose" <> O.short 'v' <> O.help "verbose logging")
  <*> O.strArgument (O.metavar "CMD" <> O.help "command entrypoint")
  <*> many (O.strArgument (O.metavar "ARG"))

withJob :: HC.Manager
  -> Opts
  -> (Job -> RIO LoggedProcessContext ExitCode)
  -> RIO LoggedProcessContext ()
withJob man Opts{..} cont = do
  logInfo "Waiting for a job"
  reqJob <- HC.parseUrlThrow $ "http://" <> host <> "/jobs/pop"
  job <- liftIO (HC.httpLbs reqJob
    { HC.responseTimeout = HC.responseTimeoutNone
    , HC.requestVersion = http20 }
    man) >>= \resp -> case J.eitherDecode (HC.responseBody resp) of
      Left e -> do
        logError $ displayShow e
        exitFailure
      Right j -> pure j
  reqStatus <- HC.parseUrlThrow $ "http://" <> host <> "/jobs/" <> T.unpack (jobId job) <> "/status"
  let sendStatus s = do
        logInfo $ display (jobId job) <> ": " <> displayShow s
        now <- liftIO getCurrentTime
        _ <- HCS.httpLbs reqStatus
          { HC.method = "POST"
          , HC.requestBody = HC.RequestBodyLBS $ J.encode $ Message now s }
        return ()
  try (cont job) >>= \case
    Left e -> sendStatus $ Errored $ T.pack $ show (e :: SomeException)
    Right ExitSuccess -> sendStatus Success
    Right (ExitFailure c) -> sendStatus $ Failed c

runJob :: Opts -> Job -> RIO LoggedProcessContext ExitCode
runJob Opts{..} Job{..} = do
  let lp = display jobId <> ": "
  let args = arguments ++ map T.unpack jobArgs
  logInfo $ lp <> "Starting " <> displayShow args
  let prefix = ".jobtower-worker"
  createDirectoryIfMissing True prefix
  withFile (prefix </> "job-" <> T.unpack jobId <.> "log") WriteMode $ \logHandle ->
    withModifyEnvVars
    (M.insert "JOBTOWER_ID" jobId)
    $ proc entrypoint args $ \conf -> withProcessWait
    ( setStdin (byteStringInput $ BL.fromStrict $ encodeUtf8 jobInput)
    $ setStdout (useHandleOpen logHandle)
    $ setStderr (useHandleOpen logHandle) conf)
    waitExitCode

main :: IO ()
main = do
  opts <- O.execParser $ O.info parseOpts mempty
  logOpts <- logOptionsHandle stdout (verbose opts)
  pcxt <- mkDefaultProcessContext
  man <- HC.newManager HC.defaultManagerSettings
  withLogFunc logOpts
    $ \lf -> runRIO (LoggedProcessContext pcxt lf)
    $ forever $ withJob man opts $ runJob opts
