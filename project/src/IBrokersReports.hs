{-# LANGUAGE OverloadedStrings #-}

module IBrokersReports (loadIBFlexReport, loadIBRecords, checkReportError, makeIBRecords, RowData) where

import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpBS )

import qualified Data.ByteString.Char8 as BS

import Text.XML.Light
    ( unqual,
      findElements,
      Attr(Attr),
      Element(elAttribs),
      QName(qName), parseXMLDoc, strContent, findElement )
import Data.Aeson (encode)

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (fromList)

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

makeIBRecords :: Element -> ByteString
makeIBRecords reportTree = encode [fromList (attrToPair a) | a <- findElements (unqual "OpenPosition") reportTree]
   where
       attrToPair e = [(qName n, v) | Attr n v <- elAttribs e]
