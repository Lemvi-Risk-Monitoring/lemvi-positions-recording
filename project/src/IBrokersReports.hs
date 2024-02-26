{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module IBrokersReports (
    handleRequest,
    handleFetch,
    findReportDate,
    ReportRequestResponse,
    ReportFetchResponse,
    CallbackBody(..),
    ReportRequestResult(..)
) where

import Network.HTTP.Simple ( parseRequest, getResponseBody, httpBS, setRequestResponseTimeout )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

import Text.XML.Light
    ( unqual,
      findElements,
      Attr(Attr),
      Element(elAttribs),
      QName(qName), parseXMLDoc, strContent, findElement, findAttr )

import Data.Map.Strict (fromList)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Control.Exception (throw, Exception)
import Data.Data (Typeable)
import Network.HTTP.Conduit ( responseTimeoutMicro )
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import qualified System.Log.FastLogger as LOG

import qualified PostSQS
import qualified Helper
import qualified AWSEvent (RecordSet(..), Record(..))

data ReportRequestResult = ReportRequestResult { referenceCode :: String, callbackURL :: String, countRetries :: Int }
  deriving (Show, Generic)
instance A.ToJSON ReportRequestResult
instance A.FromJSON ReportRequestResult

data CallbackBody = Record
    { contents      :: ReportRequestResult
    , tag           :: T.Text
    } deriving (Show, Generic)

instance A.FromJSON CallbackBody
instance A.ToJSON CallbackBody

data ReportRequestResponse = ReportRequestResponse ReportRequestResult | ReportRequestError String
  deriving Generic
instance A.ToJSON ReportRequestResponse

data ReportFetchResponse = ReportFetchResponse T.Text | ReportFetchError T.Text
  deriving Generic
instance A.ToJSON ReportFetchResponse


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
            return $ ReportRequestResponse $ ReportRequestResult { referenceCode = refCode, callbackURL = url, countRetries = 5 }
        _ -> return $ ReportRequestError ("failed to call " <> ibFlexURL' <> ", unable to parse response: " <> response)


loadIBFlexReport :: String -> String -> String -> IO String
loadIBFlexReport ibReportBaseUrl ibToken ibReportReferenceCode = do
    let ibReportURL  = ibReportBaseUrl <> "?t=" <> ibToken <> "&q=" <> ibReportReferenceCode <> "&v=3"
    loggerSet <- LOG.newStderrLoggerSet LOG.defaultBufSize
    logMessage loggerSet $ "loading report using url " <> T.pack ibReportURL
    reportRequest <- parseRequest ibReportURL
    ibReport <- httpBS reportRequest
    return $ BS.unpack (getResponseBody ibReport)


loadIBRecords :: String -> String -> String -> IO (Maybe Element)
loadIBRecords ibReportBaseUrl ibToken ibReportReferenceCode = do
    report <- loadIBFlexReport ibReportBaseUrl ibToken ibReportReferenceCode
    return $ parseXMLDoc report

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

fetchFlexReport :: String -> String -> String -> IO Element
fetchFlexReport ibReportBaseUrl ibToken ibReportReferenceCode = do
    maybeTree <- loadIBRecords ibReportBaseUrl ibToken ibReportReferenceCode
    case maybeTree of
        Just tree -> case checkReportError tree of
            Just (errorCode, errorMessage) -> throw (ReportServerError (LT.pack msg))
                where msg = "report not ready: " <> errorMessage <> " (code " <> show errorCode <> ")"
            Nothing -> return tree
        Nothing -> throw $ LoadingFailed "failed to load data"

data FlexReportEvent where
  FlexReportEvent :: {flexQueryId :: String} -> FlexReportEvent
  deriving Generic
instance A.FromJSON FlexReportEvent

handleRequest :: A.Value -> IO ReportRequestResponse
handleRequest flexReportEvent =
    case parseMaybe A.parseJSON flexReportEvent of
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

findReportDate :: Element -> Maybe T.Text
findReportDate xml = do
    xmlElem <- findElement (unqual "FlexStatement") xml
    attr <- findAttr (unqual "toDate") xmlElem
    return $ T.pack attr

handleFetch :: A.Value -> IO ReportFetchResponse
handleFetch flexReportReference = do
    loggerSet <- LOG.newStderrLoggerSet LOG.defaultBufSize
    maybeFlexReportToken <- lookupEnv "IB_FLEX_REPORT_TOKEN"
    maybeBucket <- lookupEnv "IBROKERS_BUCKET_POSITIONS"
    case (maybeFlexReportToken, maybeBucket) of
        (Just token, Just bucket) -> do
            case A.decode jsonText :: Maybe AWSEvent.RecordSet of
                Just (AWSEvent.RecordSet records) -> do
                    let
                        bodyText = AWSEvent.body $ head records
                    case (A.eitherDecode ((BSC.pack . T.unpack) bodyText) :: Either String IBrokersReports.CallbackBody) of
                        Left msg -> return $ ReportFetchError $ "failed to parse input event body: " <> T.pack msg
                        Right body -> do
                            let url = callbackURL (contents body)
                            let refCode = referenceCode (contents body)
                            -- let retries = countRetries (contents body)
                            report <- fetchFlexReport url token refCode
                            let
                                maybeReportDate = findReportDate report
                                ibRecords = BS.pack . LT.unpack $ makeIBRecords report
                            case maybeReportDate of
                                Just reportDate -> do
                                    logMessage loggerSet $ T.pack "saving records to " <> outputPath
                                    Helper.writeToS3 (T.pack bucket) outputPath ibRecords "application/json"
                                    return $ ReportFetchResponse $ T.pack "saved records to " <> outputPath
                                        where
                                            outputPath = toDatePath reportDate <> "/" <> reportDate <> ".json"
                                Nothing -> return $ ReportFetchError $ "failed to retrieve report date " <> T.pack (show report)
                Nothing -> do
                    logMessage loggerSet $ T.pack "failed to parse input event: " <> T.pack (show flexReportReference)
                    return $ ReportFetchError $ "failed to parse input event" <> T.pack (show flexReportReference)
                where jsonText = A.encode flexReportReference
        _ -> return $ ReportFetchError "required env variables: IB_FLEX_REPORT_TOKEN, IBROKERS_BUCKET_POSITIONS"

toDatePath :: T.Text -> T.Text
toDatePath dateText = year <> "/" <> month
    where
        (year, month) = T.splitAt 4 (T.take 6 dateText)

logMessage :: LOG.LoggerSet -> T.Text -> IO ()
logMessage loggerSet msg = do
    LOG.pushLogStrLn loggerSet . LOG.toLogStr $ msg
