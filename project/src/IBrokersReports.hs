{-# LANGUAGE OverloadedStrings #-}

module IBrokersReports (loadIBRecords, checkReportError, makeIBRecords, RowData) where

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

loadIBRecords :: String -> String -> IO (Maybe Element)
loadIBRecords ib_token ib_query_id = do
    let ibFlexUrl = "https://www.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest?t=" ++ ib_token ++ "&q=" ++ ib_query_id ++ "&v=3"

    request <- parseRequest ibFlexUrl
    ibResponse <- httpBS request

    let ibReportLocation = parseXMLDoc $ BS.unpack (getResponseBody ibResponse)
        ibReportReferenceCode = findReferenceCode ibReportLocation
        ibReportBaseUrl = findURL ibReportLocation
    case (ibReportReferenceCode, ibReportBaseUrl) of
        (Just refCode, Just url) -> do
            let ibReportURL  = url ++ "?t=" ++ ib_token ++ "&q=" ++ refCode ++ "&v=3"
            reportRequest <- parseRequest ibReportURL
            ibReport <- httpBS reportRequest

            let reportText = BS.unpack (getResponseBody ibReport) :: String
            return $ parseXMLDoc reportText

        _ -> return Nothing
   

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
