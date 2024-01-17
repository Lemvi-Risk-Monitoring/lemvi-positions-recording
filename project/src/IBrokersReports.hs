{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module IBrokersReports (loadIBRecords) where

import Network.HTTP.Simple
import Control.Monad
import Text.XML.HXT.Core hiding (getQName)

import qualified Text.XML.HXT.DOM.QualifiedName as QN
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as MAP

data RowData = RowData (MAP.Map String String) deriving (Show, Eq)

loadIBRecords :: String -> String -> IO [XmlTree]
loadIBRecords ib_token ib_query_id = do
    let ib_flex_url = "https://www.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest?t=" ++ ib_token ++ "&q=" ++ ib_query_id ++ "&v=3"

    request <- parseRequest ib_flex_url
    ib_response <- httpBS request

    let ib_report_location = readString [withValidate no] (BS.unpack (getResponseBody ib_response))

    ib_report_reference_code <- runX (ib_report_location >>> deep (isElem >>> hasName "ReferenceCode") //> getText)
    ib_report_base_url <- runX (ib_report_location >>> deep (isElem >>> hasName "Url") //> getText)

    let ib_report_url  = head ib_report_base_url ++ "?t=" ++ ib_token ++ "&q=" ++ head ib_report_reference_code ++ "&v=3"
    reportRequest <- parseRequest ib_report_url
    ib_report <- httpBS reportRequest

    let ib_report_content = readString [withValidate no] (BS.unpack (getResponseBody ib_report))

    ib_records <- runX $ ib_report_content
    return ib_records
