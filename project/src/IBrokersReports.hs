{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module IBrokersReports (handleRequest, handleFetch, ReportRequestResponse, ReportFetchResponse) where

import Network.HTTP.Simple ( parseRequest, getResponseBody, httpBS, setRequestResponseTimeout )
import qualified Data.ByteString.Char8 as BS

import Text.XML.Light
    ( unqual,
      findElements,
      Attr(Attr),
      Element(elAttribs),
      QName(qName), parseXMLDoc, strContent, findElement )

import Data.Map.Strict (fromList)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Control.Exception (throw, Exception)
import Data.Data (Typeable)
import Network.HTTP.Conduit ( responseTimeoutMicro )
import GHC.Generics (Generic)
import Data.Aeson ( ToJSON, FromJSON, Value, parseJSON )
import Data.Aeson.Types (parseMaybe)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

import qualified PostSQS


data ReportRequestResult = ReportRequestResult { referenceCode :: String, callbackURL :: String }
  deriving Generic
instance ToJSON ReportRequestResult

data ReportRequestResponse = ReportRequestResponse ReportRequestResult | ReportRequestError String
  deriving Generic
instance ToJSON ReportRequestResponse

data ReportFetchResponse = ReportFetchError String
  deriving Generic
instance ToJSON ReportFetchResponse

createRequestIBFlexReport :: String -> String -> String -> IO ReportRequestResponse
createRequestIBFlexReport ibFlexURL ibQueryId ibToken = do
    let ibFlexURL' = ibFlexURL <> "?t=" <> ibToken <> "&q=" <> ibQueryId <> "&v=3"
    request <- parseRequest ibFlexURL'
    let request' = setRequestResponseTimeout (responseTimeoutMicro 120000000) request
    ibResponse <- httpBS request'

    let
        response = BS.unpack (getResponseBody ibResponse)
        ibReportLocation = parseXMLDoc response
        ibReportReferenceCode = findReferenceCode ibReportLocation
        ibReportBaseUrl = findURL ibReportLocation
    case (ibReportReferenceCode, ibReportBaseUrl) of
        (Just refCode, Just url) -> do
            return $ ReportRequestResponse $ ReportRequestResult { referenceCode = refCode, callbackURL = url }
        _ -> return $ ReportRequestError ("failed to call " <> ibFlexURL' <> ", unable to parse response: " <> response)


loadIBFlexReport :: String -> String -> String -> IO (Maybe String)
loadIBFlexReport ibFlexURL ibToken ibQueryId = do
    let ibFlexURL' = ibFlexURL ++ "?t=" <> ibToken <> "&q=" <> ibQueryId <> "&v=3"
    request <- parseRequest ibFlexURL'
    let request' = setRequestResponseTimeout (responseTimeoutMicro 120000000) request
    ibResponse <- httpBS request'

    let ibReportLocation = parseXMLDoc $ BS.unpack (getResponseBody ibResponse)
        ibReportReferenceCode = findReferenceCode ibReportLocation
        ibReportBaseUrl = findURL ibReportLocation
    case (ibReportReferenceCode, ibReportBaseUrl) of
        (Just refCode, Just url) -> do
            let ibReportURL  = url <> "?t=" <> ibToken <> "&q=" <> refCode <> "&v=3"
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

makeIBRecords :: Element -> LT.Text
makeIBRecords reportTree = encodeToLazyText [fromList (attrToPair a) | a <- findElements (unqual "OpenPosition") reportTree]
   where
       attrToPair e = [(qName n, v) | Attr n v <- elAttribs e]

data IBrokersException
  = LoadingFailed !LT.Text
  | ReportServerError !LT.Text
  | EnvironmentVariableMissing !LT.Text
  deriving (Show, Typeable)

instance Exception IBrokersException

fetchFlexReport :: String -> String -> Maybe String -> IO Element
fetchFlexReport ibFlexURL flexQueryId flexReportToken = case flexReportToken of
        Just token -> do
            maybeTree <- loadIBRecords ibFlexURL token flexQueryId
            case maybeTree of
                Just tree -> case checkReportError tree of
                    Just (errorCode, errorMessage) -> throw (ReportServerError (LT.pack msg))
                        where msg = "report not ready: " <> errorMessage <> " (code " <> show errorCode <> ")"
                    Nothing -> return tree
                Nothing -> throw (LoadingFailed "failed to load data")
        Nothing -> throw (EnvironmentVariableMissing "required environment variable IB_FLEX_REPORT_TOKEN is missing")

data FlexReportEvent where
  FlexReportEvent :: {flexQueryId :: String} -> FlexReportEvent
  deriving Generic
instance FromJSON FlexReportEvent

handleRequest :: Value -> IO ReportRequestResponse
handleRequest jsonAst =
    case parseMaybe parseJSON jsonAst of
        Nothing -> return $ ReportRequestError "missing flexQueryId in request"
        Just FlexReportEvent { flexQueryId } -> do
            maybeFlexReportToken <- lookupEnv "IB_FLEX_REPORT_TOKEN"
            case maybeFlexReportToken of
                Nothing -> return $ ReportRequestError "missing env variable IB_FLEX_REPORT_TOKEN"
                Just flexReportToken -> do
                    ibFlexURLEnv <- lookupEnv "IB_FLEX_URL"
                    let ibFlexURL = fromMaybe ibFlexUrlDefault ibFlexURLEnv
                    rrr <- createRequestIBFlexReport ibFlexURL flexQueryId flexReportToken
                    maybeNotificationQueue <- lookupEnv "IBROKERS_QUEUE_REPORT_URL"
                    case maybeNotificationQueue of
                        Nothing -> return rrr
                        Just queueURL -> do
                            PostSQS.sendMessageQueueURL (T.pack queueURL) ((LT.toStrict . encodeToLazyText) rrr)
                            return rrr
    where
        ibFlexUrlDefault = "https://www.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest"

handleFetch :: Value -> IO ReportFetchResponse
handleFetch jsonAst = do
    putStrLn "not implemented"
    return $ ReportFetchError "not implemented"
