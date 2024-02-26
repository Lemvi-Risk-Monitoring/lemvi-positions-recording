{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

module AWSEvent (RecordSet) where

import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Text as T

data AttributeSet = AttributeSet
    { approximateFirstReceiveTimestamp :: T.Text
    , approximateReceiveCount          :: T.Text
    , senderId                         :: T.Text
    , sentTimestamp                    :: T.Text
    } deriving (Show, Generic)

instance A.FromJSON AttributeSet
instance A.ToJSON AttributeSet

data Record = Record
    { attributes      :: AttributeSet
    , awsRegion       :: T.Text
    , body            :: T.Text
    , eventSource     :: T.Text
    , eventSourceARN  :: T.Text
    , md5OfBody       :: T.Text
    , messageAttributes :: A.Object
    , messageId       :: T.Text
    , receiptHandle   :: T.Text
    } deriving (Show, Generic)

instance A.FromJSON Record
instance A.ToJSON Record

data RecordSet where
  Records :: {records :: [Record]} -> RecordSet
  deriving (Show, Generic)

instance A.FromJSON RecordSet
instance A.ToJSON RecordSet
