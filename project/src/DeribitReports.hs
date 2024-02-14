{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE NamedFieldPuns #-}


module DeribitReports (handler, DeribitReportResult) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text.Encoding as TE
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import qualified Data.Text.Encoding as T
import qualified Control.Lens as CL

import qualified Helper

import GHC.Generics (Generic)
import Network.HTTP.Simple (parseRequest, getResponseBody, httpBS, setRequestHeaders, getResponseStatus, setRequestQueryString)
import Data.Aeson ( ToJSON, withObject, (.:) )
import Data.Aeson.Types ( parseMaybe, Parser )
import System.Environment (lookupEnv)
import Data.String (IsString)
import Network.HTTP.Types.Status (statusIsSuccessful)
import Data.Maybe (fromMaybe)

import Amazonka.S3.PutObject (putObject_contentType)
import Data.Data (Typeable)
import Control.Exception (Exception, throw)

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

data DeribitReportResult where
  ReportContent :: [T.Text] -> DeribitReportResult
  deriving (Generic, ToJSON)

data DeribitException
  = AuthenticationFailed !T.Text
  | MissingEnvironmentVariables !T.Text
  | FailedDeribitAPI !T.Text
  | BadRequest !T.Text
  deriving (Show, Typeable)

instance Exception DeribitException

data DeribitReportEvent where
  DeribitReportEvent :: {currencies :: [T.Text]} -> DeribitReportEvent
  deriving Generic
instance A.FromJSON DeribitReportEvent

handler :: A.Value -> IO DeribitReportResult
handler jsonAst =
    case parseMaybe A.parseJSON jsonAst of
        Nothing -> throw $ BadRequest "Missing field 'currencies'"
        Just DeribitReportEvent { currencies } -> do
            deribitClientId <- lookupEnv "DERIBIT_CLIENT_ID"
            deribitClientSecret <- lookupEnv "DERIBIT_CLIENT_SECRET"
            deribitBucket <- fromMaybe "dev-deribit-positions" <$> lookupEnv "DERIBIT_BUCKET_POSITIONS"
            case (deribitClientId, deribitClientSecret, deribitBucket) of
                (Just deribitClientId', Just deribitClientSecret', bucket) -> do
                  let
                    clientId = DeribitClientId $ BSC.pack deribitClientId'
                    clientSecret = DeribitClientSecret $ BSC.pack deribitClientSecret'
                  credentials <- authorizeWithCredentials clientId clientSecret publicURL
                  case credentials of
                    Nothing ->  throw $ AuthenticationFailed "authentication with deribit failed"
                    Just (token, _) -> ReportContent <$> mapM (savePositions token privateURL (T.pack bucket)) currencies
                _ -> throw $ MissingEnvironmentVariables "error: environment variables DERIBIT_CLIENT_ID, DERIBIT_CLIENT_SECRET and DERIBIT_BUCKET_POSITIONS are required"

    where
        hostName = "www.deribit.com"
        deribitEndpointV2 = "https://" ++ hostName ++ "/api/v2"
        publicURL = deribitEndpointV2 <> "/public"
        privateURL = deribitEndpointV2 <> "/private"

savePositions :: BS.ByteString -> String -> T.Text -> T.Text -> IO T.Text
savePositions token privateURL bucket currencyCode = do
    (year, month, day) <- Helper.today
    request <- parseRequest $ privateURL <> "/get_positions"
    let
      path = T.pack $ Helper.formatDate "/" (year, month, day)
      filename = T.pack $ "positions-" <> Helper.formatDate "-" (year, month, day) <> "-" <> T.unpack currencyCode <> ".json"
      headers = [
        ("Content-Type", "application/json"),
        ("Authorization", "Bearer " <> token)
        ]
      withHeaders = setRequestHeaders headers request

      queryParams = [
          ("currency", Just $ T.encodeUtf8 currencyCode)
        ]
      withBodyParams = setRequestQueryString queryParams withHeaders
    ibResponse <- httpBS withBodyParams
    let
      responseBody = getResponseBody ibResponse
      responseStatus = getResponseStatus ibResponse
      success = statusIsSuccessful responseStatus
    if success then
        case A.decodeStrict responseBody :: Maybe A.Value of
          Just (A.Object json) -> (
            if KM.member "result" json then (do
              let
              writeToS3 (S3.BucketName bucket) (S3.ObjectKey (path <> "/" <> filename)) (AWS.toBody responseBody) "application/json"
              return $ TE.decodeUtf8 responseBody
              )
            else
              throw $ FailedDeribitAPI $ "no result found in body: " <> TE.decodeUtf8 responseBody)
          _ -> throw $ FailedDeribitAPI $ "failed to parse body: " <> TE.decodeUtf8 responseBody
      else
        throw $ FailedDeribitAPI $ "failed to load positions: " <> TE.decodeUtf8 responseBody

writeToS3 :: S3.BucketName -> S3.ObjectKey -> AWS.RequestBody -> T.Text -> IO ()
writeToS3 bucket filename json objectType = do
  env <- AWS.newEnv AWS.discover
  let
    request = S3.newPutObject bucket filename json CL.& putObject_contentType CL.?~ objectType
  _ <- AWS.runResourceT $ AWS.send env request
  return ()
