{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

import AWS.Lambda.Runtime (mRuntime)
import Data.Aeson ( FromJSON, ToJSON, FromJSON(parseJSON) )
import GHC.Generics       (Generic)
import Data.Aeson.Types (Value, parseMaybe)
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import DeribitReports (authorizeWithCredentials)

data DeribitReportResult where
  DeribitReportResult :: {message :: String} -> DeribitReportResult
  deriving Generic
instance ToJSON DeribitReportResult

data DeribitReportEvent where
  FlexReportEvent :: {clientId :: String} -> DeribitReportEvent
  deriving Generic
instance FromJSON DeribitReportEvent

handler :: Value -> IO DeribitReportResult
handler _ = do
            deribitClientId <- lookupEnv "DERIBIT_CLIENT_ID"
            deribitClientSecret <- lookupEnv "DERIBIT_CLIENT_SECRET"
            case (deribitClientId, deribitClientSecret) of
                (Just deribitClientId', Just deribitClientSecret') -> do
                  (content, _) <- authorizeWithCredentials deribitClientId' deribitClientSecret' publicURL
                  return $ DeribitReportResult { message = content }
                _ -> return $ DeribitReportResult { message = "error" }
                    
    where
        hostName = "www.deribit.com"
        deribitEndpointV2 = "https://" ++ hostName ++ "/api/v2"
        publicURL = deribitEndpointV2 ++ "/public"
        privateURL = deribitEndpointV2 ++ "/private"

main :: IO ()
main = mRuntime handler
