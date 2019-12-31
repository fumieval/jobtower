{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import RIO

import Control.Monad.STM (retry)
import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdout)
import RIO.Directory (createDirectoryIfMissing, doesFileExist)
import RIO.FilePath ((</>))
import JobTower.Types
import qualified Data.Aeson as J
import qualified Data.Heap as H
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified RIO.ByteString as B
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types

writeFileChunks :: FilePath -> IO ByteString -> IO ()
writeFileChunks path pop = withFile path WriteMode go where
  go h = do
    bs <- pop
    unless (B.null bs) $ B.hPutStr h bs >> go h

app :: TVar (H.Heap Job) -> LogFunc -> Application
app vQueue _logFunc req sendResp = case (requestMethod req, pathInfo req) of
  ("POST", ["files"]) -> do
    uuid <- liftIO UUID.nextRandom
    writeFileChunks (filesPrefix </> UUID.toString uuid) (getRequestBodyChunk req)
    text status200 $ encodeUtf8Builder $ UUID.toText uuid
  ("GET", ["files", name]) -> sendResp
    $ responseFile status200 [] (filesPrefix </> T.unpack name) Nothing
  ("PUT", ["files", name]) -> do
    writeFileChunks (filesPrefix </> T.unpack name) (getRequestBodyChunk req)
    text status200 "ok"
  ("HEAD", ["files", name]) -> do
    e <- doesFileExist (filesPrefix </> T.unpack name)
    if e then text status200 "" else text status404 ""
  ("POST", ["jobs"]) -> strictRequestBody req >>= \bs -> case J.eitherDecode bs of
    Left err -> text status400 (fromString err)
    Right jobs -> do
      jobs' <- forM jobs $ \j -> do
        uuid <- liftIO $ UUID.toText <$> UUID.nextRandom
        t <- liftIO $ maybe getCurrentTime pure $ jobTime j
        pure j { jobId = uuid, jobTime = Just t }
      liftIO $ atomically $ modifyTVar' vQueue $ foldr H.insert `flip` (jobs' :: [Job])
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
  createDirectoryIfMissing True filesPrefix
  logOpts <- logOptionsHandle stderr True
  withLogFunc logOpts $ \lf -> Warp.run 1837 (logStdout $ app vQueue lf) `finally` do
    jobs <- readTVarIO vQueue
    BL.writeFile (prefix </> "dump.json") $ J.encode $ toList jobs
