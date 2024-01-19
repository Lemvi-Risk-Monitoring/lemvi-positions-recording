{-# LANGUAGE OverloadedStrings #-}

module IBrokersReports (loadIBFlexReport, loadIBRecords, checkReportError, makeIBRecords, fetchFlexReport, RowData) where

import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpBS )

import qualified Data.ByteString.Char8 as BS

import Text.XML.Light
    ( unqual,
      findElements,
      Attr(Attr),
      Element(elAttribs),
      QName(qName), parseXMLDoc, strContent, findElement )

import Data.Map.Strict (fromList)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (Text, pack)
import Control.Exception (throw, Exception)
import Data.Data (Typeable)

loadIBFlexReport :: String -> String -> String -> IO (Maybe String)
loadIBFlexReport ibFlexURL ibToken ibQueryId = do
    let ibFlexUrl = ibFlexURL ++ "?t=" ++ ibToken ++ "&q=" ++ ibQueryId ++ "&v=3"

    request <- parseRequest ibFlexUrl
    ibResponse <- httpBS request

    let ibReportLocation = parseXMLDoc $ BS.unpack (getResponseBody ibResponse)
        ibReportReferenceCode = findReferenceCode ibReportLocation
        ibReportBaseUrl = findURL ibReportLocation
    case (ibReportReferenceCode, ibReportBaseUrl) of
        (Just refCode, Just url) -> do
            let ibReportURL  = url ++ "?t=" ++ ibToken ++ "&q=" ++ refCode ++ "&v=3"
            reportRequest <- parseRequest ibReportURL
            ibReport <- httpBS reportRequest
            return $ Just $ BS.unpack (getResponseBody ibReport)

        _ -> return Nothing

loadIBRecords :: String -> String -> String -> IO (Maybe Element)
loadIBRecords ibFlexURL ibToken ibQueryId = do
    report <- loadIBFlexReport ibFlexURL ibToken ibQueryId
    return $ parseXMLDoc =<< report

findReferenceCode :: Maybe Element -> Maybe String
findReferenceCode (Just tree) = strContent <$> findElement (unqual "ReferenceCode") tree
findReferenceCode _ = Nothing

findURL :: Maybe Element -> Maybe String
findURL (Just tree) = strContent <$> findElement (unqual "Url") tree
findURL _ = Nothing

type ErrorCode = Int
type ErrorMessage = String

findErrorCode :: Element -> Maybe ErrorCode
findErrorCode tree = read . strContent <$> findElement (unqual "ErrorCode") tree

findErrorMessage :: Element -> Maybe ErrorMessage
findErrorMessage tree = strContent <$> findElement (unqual "ErrorMessage") tree

checkReportError :: Element -> Maybe (ErrorCode, ErrorMessage)
checkReportError tree = case (findErrorCode tree, findErrorMessage tree) of
    (Just errorCode, Just errorMessage) -> Just (errorCode, errorMessage)
    _ -> Nothing

type FieldName = String
type FieldValue = String
type RowData = [(FieldName, FieldValue)]

makeIBRecords :: Element -> Text
makeIBRecords reportTree = encodeToLazyText [fromList (attrToPair a) | a <- findElements (unqual "OpenPosition") reportTree]
   where
       attrToPair e = [(qName n, v) | Attr n v <- elAttribs e]
       
data IBrokersException
  = LoadingFailed !Text
  | ReportServerError !Text
  | EnvironmentVariableMissing !Text
  deriving (Show, Typeable)

instance Exception IBrokersException

fetchFlexReport :: String -> Maybe String -> Maybe String -> IO Element
fetchFlexReport ibFlexURL flexQueryId flexReportToken = case (flexQueryId, flexReportToken) of
        (Just qId, Just token) -> do
            maybeTree <- loadIBRecords ibFlexURL token qId
            case maybeTree of
                Just tree -> case checkReportError tree of
                    Just (errorCode, errorMessage) -> throw (ReportServerError (pack msg))
                        where msg = "report not ready: " ++ errorMessage ++ " (code " ++ show errorCode ++ ")"
                    Nothing -> return tree
                Nothing -> throw (LoadingFailed "failed to load data")
        (Just _, Nothing) -> throw (EnvironmentVariableMissing "required environment variable IB_FLEX_REPORT_TOKEN is missing")
        (Nothing, Just _) -> throw (EnvironmentVariableMissing "required environment variable IB_FLEX_QUERY_ID is missing")
        (Nothing, Nothing) -> throw (EnvironmentVariableMissing "required environment variables IB_FLEX_QUERY_ID and IB_FLEX_REPORT_TOKEN are missing")
