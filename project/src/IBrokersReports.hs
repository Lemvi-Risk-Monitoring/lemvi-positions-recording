{-# LANGUAGE OverloadedStrings #-}

module IBrokersReports (loadIBRecords, checkReportError) where

import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpBS )

import qualified Data.ByteString.Char8 as BS
import Text.Read (readMaybe)
import Control.Arrow.ListArrow (runLA)
import Text.XML.HXT.Core (XmlTree, ArrowXml (hasName, getText), 
    ArrowTree ((/>), deep, (//>)), 
    readString, withValidate, no, runX, (>>>), 
    isElem, withParseHTML, withWarnings)
import Data.Maybe (listToMaybe)

loadIBRecords :: String -> String -> IO (Maybe XmlTree)
loadIBRecords ib_token ib_query_id = do
    let ibFlexUrl = "https://www.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest?t=" ++ ib_token ++ "&q=" ++ ib_query_id ++ "&v=3"

    request <- parseRequest ibFlexUrl
    ibResponse <- httpBS request

    let ibReportLocation = readString [withValidate no] (BS.unpack (getResponseBody ibResponse))

    ibReportReferencCode <- runX (ibReportLocation >>> deep (isElem >>> hasName "ReferenceCode") //> getText)
    ibReportBaseUrl <- runX (ibReportLocation >>> deep (isElem >>> hasName "Url") //> getText)

    let ib_report_url  = head ibReportBaseUrl ++ "?t=" ++ ib_token ++ "&q=" ++ head ibReportReferencCode ++ "&v=3"
    reportRequest <- parseRequest ib_report_url
    ibReport <- httpBS reportRequest

    let reportText = BS.unpack (getResponseBody ibReport) :: String

    let ibReportContent = readString [withValidate no, withParseHTML no, withWarnings no] reportText

    trees <- runX ibReportContent
    case trees of
        [] -> return Nothing
        tree:_ -> return $ Just tree

type ErrorCode = Int
type ErrorMessage = String

getErrorCodeText :: (ArrowXml a) => a XmlTree String
getErrorCodeText = hasName "ErrorCode" /> getText

getErrorMessageText :: (ArrowXml a) => a XmlTree String
getErrorMessageText = hasName "ErrorMessage" /> getText

checkReportError :: XmlTree -> Maybe (ErrorCode, ErrorMessage)
checkReportError xmlTree = do
  errorCodeStr <- listToMaybe . runLA getErrorCodeText $ xmlTree
  errorMessage <- listToMaybe . runLA getErrorMessageText $ xmlTree
  errorCode <- readMaybe errorCodeStr
  return (errorCode, errorMessage)
