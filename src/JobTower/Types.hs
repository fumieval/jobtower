{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module JobTower.Types where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Data.Function (on)
import System.FilePath ((</>))

data Job = Job
  { jobId :: Text
  , jobTime :: Maybe UTCTime
  , jobFiles :: [Text]
  , jobCmd :: Text
  , jobArgs :: [Text]
  , jobInput :: Text
  } deriving (Show, Generic)

instance FromJSON Job where
  parseJSON = withObject "Job" $ \obj -> Job
    <$> obj .:? "id" .!= ""
    <*> obj .:? "time"
    <*> obj .:? "files" .!= []
    <*> obj .: "cmd"
    <*> obj .:? "args" .!= []
    <*> obj .:? "input" .!= ""

instance ToJSON Job where
  toJSON Job{..} = object
    [ "id" .= jobId
    , "time" .= jobTime
    , "files" .= jobFiles
    , "cmd" .= jobCmd
    , "args" .= jobArgs
    , "input" .= jobInput
    ]
instance Eq Job where
  (==) = on (==) jobTime
instance Ord Job where
  compare = on compare jobTime

prefix :: FilePath
prefix = ".jobtower"

filesPrefix :: FilePath
filesPrefix = prefix </> "files"
