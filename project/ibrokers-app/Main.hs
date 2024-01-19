{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment ( lookupEnv )

import IBrokersReports (loadIBRecords, checkReportError, makeIBRecords)
import Control.Exception (Exception, throw)
import Data.Data (Typeable)
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)

data IBrokersException
  = LoadingFailed !Text
  | ReportServerError !Text
  | EnvironmentVariableMissing !Text
  deriving (Show, Typeable)

instance Exception IBrokersException


main :: IO ()
main = do
    let ibFlexUrlDefault = "https://www.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest"
    flexQueryId <- lookupEnv "IB_FLEX_QUERY_ID"
    flexReportToken <- lookupEnv "IB_FLEX_REPORT_TOKEN"
    ibFlexURL <- lookupEnv "IB_FLEX_URL"
    case (flexQueryId, flexReportToken) of
        (Just qId, Just token) -> do
            maybeTree <- loadIBRecords (fromMaybe ibFlexUrlDefault ibFlexURL) token qId
            case maybeTree of
                Just tree -> case checkReportError tree of
                    Just (errorCode, errorMessage) -> throw (ReportServerError (pack msg))
                        where msg = "report not ready: " ++ errorMessage ++ " (code " ++ show errorCode ++ ")"
                    Nothing -> print $ makeIBRecords tree
                Nothing -> throw (LoadingFailed "failed to load data")
        (Just _, Nothing) -> throw (EnvironmentVariableMissing "required environment variable IB_FLEX_REPORT_TOKEN is missing")
        (Nothing, Just _) -> throw (EnvironmentVariableMissing "required environment variable IB_FLEX_QUERY_ID is missing")
        (Nothing, Nothing) -> throw (EnvironmentVariableMissing "required environment variables IB_FLEX_QUERY_ID and IB_FLEX_REPORT_TOKEN are missing")
