module Main (main) where

import System.Environment

import IBrokersReports (loadIBRecords)

main :: IO ()
main = do
    flexQueryId <- lookupEnv "IB_FLEX_QUERY_ID"
    flexReportToken <- lookupEnv "IB_FLEX_REPORT_TOKEN"
    case (flexQueryId, flexReportToken) of
        (Just qId, Just token) -> do
            positions <- loadIBRecords token qId
            print positions
        _ -> putStrLn "required environment variables: IB_FLEX_QUERY_ID and IB_FLEX_REPORT_TOKEN"
    
