{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import System.Environment (lookupEnv)
import IBrokersReports (fetchFlexReport, makeIBRecords)
import Data.Maybe (fromMaybe)

import AWS.Lambda.Runtime (mRuntime)
import Data.Aeson ( FromJSON, ToJSON, FromJSON(parseJSON) )
import GHC.Generics       (Generic)
import Data.Aeson.Types (Value, parseMaybe)
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)

data FlexReportResult where
  FlexReportResult :: {message :: String} -> FlexReportResult
  deriving Generic
instance ToJSON FlexReportResult

data FlexReportEvent where
  FlexReportEvent :: {flexQueryId :: String} -> FlexReportEvent
  deriving Generic
instance FromJSON FlexReportEvent

handler :: Value -> IO FlexReportResult
handler jsonAst =
    case parseMaybe parseJSON jsonAst of
        Nothing -> return FlexReportResult { message = "Missing flexQueryId" }
        Just FlexReportEvent { flexQueryId } -> do
            flexReportToken <- lookupEnv "IB_FLEX_REPORT_TOKEN"
            ibFlexURLEnv <- lookupEnv "IB_FLEX_URL"
            let ibFlexURL = fromMaybe ibFlexUrlDefault ibFlexURLEnv
            reportTree <- fetchFlexReport ibFlexURL flexQueryId flexReportToken
            return $ FlexReportResult { message = (unpack . toStrict . makeIBRecords) reportTree }
    where
        ibFlexUrlDefault = "https://www.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest"

main :: IO ()
main = mRuntime handler
