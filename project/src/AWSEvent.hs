{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module AWSEvent (RecordSet(..), Record(..)) where

import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Text as T

data AttributeSet = AttributeSet
    { approximateFirstReceiveTimestamp :: T.Text
    , approximateReceiveCount          :: T.Text
    , senderId                         :: T.Text
    , sentTimestamp                    :: T.Text
    } deriving (Show, Generic)

instance A.FromJSON AttributeSet where
    parseJSON :: AT.Value -> AT.Parser AttributeSet
    parseJSON = A.withObject "AttributeSet" $ \o -> do
        afrt <- o A..: "ApproximateFirstReceiveTimestamp"
        arc <- o A..: "ApproximateReceiveCount"
        sid <- o A..: "SenderId"
        sts <- o A..: "SentTimestamp"
        return $ AttributeSet afrt arc sid sts

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
  RecordSet :: {records :: [Record]} -> RecordSet
  deriving (Show, Generic)

instance A.FromJSON RecordSet where
    parseJSON :: AT.Value -> AT.Parser RecordSet
    parseJSON = A.withObject "RecordSet" $ \o -> do
        recs <- o A..: "Records"
        return $ RecordSet recs

instance A.ToJSON RecordSet
