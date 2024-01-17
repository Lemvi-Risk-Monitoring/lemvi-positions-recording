module Main (main) where

import System.Environment ( lookupEnv )

import IBrokersReports (loadIBRecords, checkReportError, makeIBRecords)
import Text.Printf ( printf )

main :: IO ()
main = do
    flexQueryId <- lookupEnv "IB_FLEX_QUERY_ID"
    flexReportToken <- lookupEnv "IB_FLEX_REPORT_TOKEN"
    case (flexQueryId, flexReportToken) of
        (Just qId, Just token) -> do
            maybeTree <- loadIBRecords token qId
            case maybeTree of
                Just tree -> case checkReportError tree of
                    Just (errorCode, errorMessage) -> printf "Error: failed to load data: %s (code %d)\n" errorMessage errorCode
                    Nothing -> print $ makeIBRecords tree
                Nothing -> putStrLn "failed to load data"
        _ -> putStrLn "required environment variables: IB_FLEX_QUERY_ID and IB_FLEX_REPORT_TOKEN"
