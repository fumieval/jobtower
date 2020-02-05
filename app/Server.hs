{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

import Control.Monad.STM (retry)
import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdout)
import JobTower.Types
import qualified Data.Aeson as J
import qualified Data.Heap as H
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.Wai.Handler.Warp as Warp
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types
import System.Environment (lookupEnv)

app :: TVar (H.Heap Job) -> LogFunc -> Application
app vQueue logFunc req sendResp = case (requestMethod req, pathInfo req) of
  ("POST", ["jobs"]) -> strictRequestBody req >>= \bs -> case J.eitherDecode bs of
    Left err -> text status400 (fromString err)
    Right jobs -> do
      jobs' <- forM jobs $ \j -> do
        uuid <- liftIO $ UUID.toText <$> UUID.nextRandom
        t <- liftIO $ maybe getCurrentTime pure $ jobTime j
        pure j { jobId = uuid, jobTime = Just t }
      liftIO $ atomically $ modifyTVar' vQueue $ foldr H.insert `flip` (jobs' :: [Job])
      text status200 "ok"
  ("POST", ["jobs", jid, "status"]) -> do
    bs <- strictRequestBody req
    runRIO logFunc $ logInfo $ display jid <> ": " <> displayShow bs
    text status200 "ok"
  ("GET", ["jobs"]) -> do
    jobs <- liftIO $ readTVarIO vQueue
    json $ toList jobs
  ("GET", ["jobs", "pop"]) -> do
    job <- liftIO $ atomically $ do
      H.viewMin <$> readTVar vQueue >>= \case
        Nothing -> retry
        Just (x, h) -> x <$ writeTVar vQueue h
    json job `onException` do atomically $ modifyTVar' vQueue (H.insert job)
  _ -> text status404 "Not found"
  where
    text st = sendResp . responseBuilder st [(hContentType, "text/plain")]
    json :: J.ToJSON a => a -> IO ResponseReceived
    json = sendResp
      . responseBuilder status200 [(hContentType, "application/json")]
      . J.fromEncoding . J.toEncoding

main :: IO ()
main = do
  vQueue <- newTVarIO (H.empty :: H.Heap Job)
  logOpts <- logOptionsHandle stderr True
  port <- (fromMaybe 1837 . (>>=readMaybe)) <$> lookupEnv "PORT"
  withLogFunc logOpts $ \lf -> do
    runRIO lf $ logInfo $ "Listening on " <> display port
    Warp.run port (logStdout $ app vQueue lf)
