{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module IBrokersReports (app) where

import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpBS, setRequestResponseTimeout )

import qualified Data.ByteString.Char8 as BS

import Text.XML.Light
    ( unqual,
      findElements,
      Attr(Attr),
      Element(elAttribs),
      QName(qName), parseXMLDoc, strContent, findElement )

import Data.Map.Strict (fromList)
import Data.Aeson.Text (encodeToLazyText)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy ( Text, pack, toStrict )
import Control.Exception (throw, Exception)
import Data.Data (Typeable)
import Network.HTTP.Conduit ( responseTimeoutMicro )
import GHC.Generics (Generic)
import Data.Aeson ( ToJSON, FromJSON, Value, parseJSON )
import Data.Aeson.Types (parseMaybe)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Servant
    ( Proxy(Proxy), (:>), JSON, Post, ReqBody, Handler, serve )
import Servant.Server (Application)

loadIBFlexReport :: String -> String -> String -> IO (Maybe String)
loadIBFlexReport ibFlexURL ibToken ibQueryId = do
    let ibFlexURL' = ibFlexURL ++ "?t=" ++ ibToken ++ "&q=" ++ ibQueryId ++ "&v=3"
    request <- parseRequest ibFlexURL'
    let request' = setRequestResponseTimeout (responseTimeoutMicro 120000000) request
    ibResponse <- httpBS request'

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

fetchFlexReport :: String -> String -> Maybe String -> IO Element
fetchFlexReport ibFlexURL flexQueryId flexReportToken = case flexReportToken of
        Just token -> do
            maybeTree <- loadIBRecords ibFlexURL token flexQueryId
            case maybeTree of
                Just tree -> case checkReportError tree of
                    Just (errorCode, errorMessage) -> throw (ReportServerError (pack msg))
                        where msg = "report not ready: " ++ errorMessage ++ " (code " ++ show errorCode ++ ")"
                    Nothing -> return tree
                Nothing -> throw (LoadingFailed "failed to load data")
        Nothing -> throw (EnvironmentVariableMissing "required environment variable IB_FLEX_REPORT_TOKEN is missing")

data FlexReportResult where
  FlexReportResult :: {message :: String} -> FlexReportResult
  deriving Generic
instance ToJSON FlexReportResult

data FlexReportEvent where
  FlexReportEvent :: {flexQueryId :: String} -> FlexReportEvent
  deriving Generic
instance FromJSON FlexReportEvent

handler :: Value -> IO FlexReportResult
handler jsonAst =
    case parseMaybe parseJSON jsonAst of
        Nothing -> return FlexReportResult { message = "Missing flexQueryId" }
        Just FlexReportEvent { flexQueryId } -> do
            flexReportToken <- lookupEnv "IB_FLEX_REPORT_TOKEN"
            ibFlexURLEnv <- lookupEnv "IB_FLEX_URL"
            let ibFlexURL = fromMaybe ibFlexUrlDefault ibFlexURLEnv
            reportTree <- fetchFlexReport ibFlexURL flexQueryId flexReportToken
            return $ FlexReportResult { message = (unpack . toStrict . makeIBRecords) reportTree }
    where
        ibFlexUrlDefault = "https://www.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest"

type Api =
  "local"
    :> ReqBody '[JSON] Value
    :> Post '[JSON] FlexReportResult

handlers :: Value -> Handler FlexReportResult
handlers = liftIO . handler

app :: Application
app = serve (Proxy :: Proxy Api) handlers
