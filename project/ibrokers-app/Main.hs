{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import System.Environment (lookupEnv, getEnv)
import IBrokersReports (fetchFlexReport, makeIBRecords)
import Data.Maybe (fromMaybe)

import AWS.Lambda.Runtime (mRuntime)
import Data.Aeson ( FromJSON, ToJSON, FromJSON(parseJSON) )
import GHC.Generics       (Generic)
import Data.Aeson.Types (Value, parseMaybe)
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)

data FlexReportResult = FlexReportResult { message :: String } deriving Generic
instance ToJSON FlexReportResult

data FlexReportEvent = FlexReportEvent {
    flexQueryId :: String
} deriving Generic
instance FromJSON FlexReportEvent

handler :: Value -> IO String
handler jsonAst =
    case parseMaybe parseJSON jsonAst of
        Nothing -> return "Missing flexQueryId"
        Just FlexReportEvent { flexQueryId } -> do
            flexReportToken <- lookupEnv "IB_FLEX_REPORT_TOKEN"
            ibFlexURLEnv <- lookupEnv "IB_FLEX_URL"
            let ibFlexURL = fromMaybe ibFlexUrlDefault ibFlexURLEnv
            reportTree <- fetchFlexReport ibFlexURL flexQueryId flexReportToken
            return $ (unpack . toStrict . makeIBRecords) reportTree
    where
        ibFlexUrlDefault = "https://www.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest"

main :: IO ()
main = mRuntime handler

main' :: IO ()
main' = do
    let ibFlexUrlDefault = "https://www.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest"
    flexQueryId <- getEnv "IB_FLEX_QUERY_ID"
    flexReportToken <- lookupEnv "IB_FLEX_REPORT_TOKEN"
    ibFlexURLEnv <- lookupEnv "IB_FLEX_URL"
    let ibFlexURL = fromMaybe ibFlexUrlDefault ibFlexURLEnv
    reportTree <- fetchFlexReport ibFlexURL flexQueryId flexReportToken
    print $ makeIBRecords reportTree
