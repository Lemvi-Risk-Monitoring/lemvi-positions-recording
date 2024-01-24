{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}


module DeribitReports (app) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.Encoding as TE

import GHC.Generics (Generic)
import Network.HTTP.Simple (parseRequest, getResponseBody, httpBS, setRequestHeaders, getResponseStatus, setRequestQueryString)
import Data.Aeson ( ToJSON, withObject, (.:) )
import Data.Aeson.Types ( parseMaybe, Parser )
import System.Environment (lookupEnv)
import Servant (Proxy(Proxy), (:>), JSON, Post, ReqBody)
import Servant.Server (Application, serve, Handler)
import Control.Monad.IO.Class (liftIO)
import Data.String (IsString)
import Network.HTTP.Types.Status (statusIsSuccessful)
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3

newtype DeribitClientId = DeribitClientId {getClientId :: BS.ByteString} deriving newtype (IsString, Show)
newtype DeribitClientSecret = DeribitClientSecret {getClientSecret ::BS.ByteString} deriving newtype (IsString, Show)

data TokenResponse = TokenResponse
  {
    accessToken :: String,
    refreshToken :: String
  } deriving (Show)

tokenParser :: A.Value -> Parser TokenResponse
tokenParser = withObject "AccessTokenResponse" $ \obj -> do
  resultObj <- obj .: "result"
  TokenResponse
    <$> resultObj .: "access_token"
    <*> resultObj .: "refresh_token"

authorizeWithCredentials :: DeribitClientId -> DeribitClientSecret -> String -> IO (Maybe (BS.ByteString, BS.ByteString))
authorizeWithCredentials deribitClientId deribitClientSecret publicURL = do
    request <- parseRequest $ publicURL <> "/auth"
    let
        headers = [("Content-Type", "application/json")]
        withHeaders = setRequestHeaders headers request
        queryParams = [
            ("client_id", Just (getClientId deribitClientId)),
            ("client_secret", Just (getClientSecret deribitClientSecret)),
            ("grant_type", Just "client_credentials")
          ]
        withAuthQuery = setRequestQueryString queryParams withHeaders
    ibResponse <- httpBS withAuthQuery
    let maybeAccessToken = parseMaybe tokenParser =<< A.decodeStrict (getResponseBody ibResponse)
    case maybeAccessToken of
      Just tokenResponse -> return $ Just (BSC.pack $ accessToken tokenResponse, BSC.pack $ refreshToken tokenResponse)
      Nothing -> return Nothing

data DeribitReportResult =
  ReportContent T.Text
  | ReportError T.Text
  deriving (Generic, ToJSON)

handler :: A.Value -> IO DeribitReportResult
handler _ = do
            deribitClientId <- lookupEnv "DERIBIT_CLIENT_ID"
            deribitClientSecret <- lookupEnv "DERIBIT_CLIENT_SECRET"
            case (deribitClientId, deribitClientSecret) of
                (Just deribitClientId', Just deribitClientSecret') -> do
                  credentials <- authorizeWithCredentials (DeribitClientId (BSC.pack deribitClientId')) (DeribitClientSecret (BSC.pack deribitClientSecret')) publicURL
                  case credentials of
                    Nothing ->  return $ ReportError "authentication failed"
                    Just (token, _) -> savePositions token privateURL "BTC"
                _ -> return $ ReportError "error: environment variables DERIBIT_CLIENT_ID and DERIBIT_CLIENT_SECRET are required"

    where
        hostName = "www.deribit.com"
        deribitEndpointV2 = "https://" ++ hostName ++ "/api/v2"
        publicURL = deribitEndpointV2 <> "/public"
        privateURL = deribitEndpointV2 <> "/private"

savePositions :: BS.ByteString -> String -> BS.ByteString -> IO DeribitReportResult
savePositions token privateURL currencyCode = do
    request <- parseRequest $ privateURL <> "/get_positions"
    let
        headers = [
          ("Content-Type", "application/json"),
          ("Authorization", "Bearer " <> token)
          ]
        withHeaders = setRequestHeaders headers request

        queryParams = [
            ("currency", Just currencyCode)
          ]
        withBodyParams = setRequestQueryString queryParams withHeaders
    ibResponse <- httpBS withBodyParams
    let
      responseBody = getResponseBody ibResponse
      responseStatus = getResponseStatus ibResponse
      success = statusIsSuccessful responseStatus
    if success then
        case A.decodeStrict responseBody :: Maybe (A.Value) of
          Just (A.Object json) -> (if KM.member "result" json then (do
                  writeToS3 (TE.decodeUtf8 responseBody)
                  return $ ReportContent (TE.decodeUtf8 responseBody))
                else
                  return $ ReportError $ "no result found in body: " <> TE.decodeUtf8 responseBody)
          _ -> return $ ReportError $ "failed to parse body: " <> TE.decodeUtf8 responseBody        
      else
      return $ ReportError $ "failed to load positions: " <> TE.decodeUtf8 responseBody

writeToS3 :: T.Text -> IO ()
writeToS3 json = do
  env <- AWS.newEnv AWS.discover
  resp <- AWS.runResourceT $ AWS.send env S3.newListBuckets
  print $ resp
  return ()

handlers :: A.Value -> Handler DeribitReportResult
handlers = liftIO . handler

type Api =
  "endpoint"
    :> ReqBody '[JSON] A.Value
    :> Post '[JSON] DeribitReportResult

app :: Application
app = serve (Proxy :: Proxy Api) handlers
