{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module JobTower.Types where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import Data.Function (on)

data Job = Job
  { jobId :: Text -- ^ Job ID (automatically assigned)
  , jobTime :: Maybe UTCTime -- ^ Job priority
  , jobArgs :: [Text] -- ^ Arguments
  , jobInput :: Text -- ^ stdin
  } deriving (Show, Generic)

instance FromJSON Job where
  parseJSON = withObject "Job" $ \obj -> Job
    <$> obj .:? "id" .!= ""
    <*> obj .:? "time"
    <*> obj .:? "args" .!= []
    <*> obj .:? "input" .!= ""

instance ToJSON Job where
  toJSON Job{..} = object
    [ "id" .= jobId
    , "time" .= jobTime
    , "args" .= jobArgs
    , "input" .= jobInput
    ]
instance Eq Job where
  (==) = on (==) jobTime
instance Ord Job where
  compare = on compare jobTime

data Message a = Message
  { msgTime :: !UTCTime
  , msgContent :: !a
  }

instance FromJSON a => FromJSON (Message a) where
  parseJSON = withObject "Message" $ \obj -> Message
    <$> obj .: "time"
    <*> obj .: "content"

instance ToJSON a => ToJSON (Message a) where
  toJSON Message{..} = object
    [ "time" .= msgTime
    , "content" .= msgContent
    ]

data JobStatus = Success
  | Failed !Int
  | Errored !Text
  | Running
  | Taken
  deriving (Generic, Show)
instance FromJSON JobStatus
instance ToJSON JobStatus
