{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (lookupEnv)
import IBrokersReports (fetchFlexReport, makeIBRecords)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    let ibFlexUrlDefault = "https://www.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest"
    flexQueryId <- lookupEnv "IB_FLEX_QUERY_ID"
    flexReportToken <- lookupEnv "IB_FLEX_REPORT_TOKEN"
    ibFlexURLEnv <- lookupEnv "IB_FLEX_URL"
    let ibFlexURL = fromMaybe ibFlexUrlDefault ibFlexURLEnv
    reportTree <- fetchFlexReport ibFlexURL flexQueryId flexReportToken
    print $ makeIBRecords reportTree
