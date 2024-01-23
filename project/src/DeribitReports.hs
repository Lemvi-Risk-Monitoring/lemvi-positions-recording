{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DeribitReports (app) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import GHC.Generics       (Generic)
import Network.HTTP.Simple (parseRequest, getResponseBody, httpBS, setRequestHeaders)
import Data.Aeson ( Value, decodeStrict, ToJSON, withObject, (.:) )
import Data.Aeson.Types ( parseMaybe, Parser )
import System.Environment (lookupEnv)
import Servant (Proxy(Proxy), (:>), JSON, Post, ReqBody)
import Servant.Server (Application, serve, Handler)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client (setQueryString)
import Data.String (IsString)

newtype DeribitClientId = DeribitClientId {getClientId :: BS.ByteString} deriving newtype (IsString, Show)
newtype DeribitClientSecret = DeribitClientSecret {getClientSecret ::BS.ByteString} deriving newtype (IsString, Show)

authorizeWithCredentials :: DeribitClientId -> DeribitClientSecret -> String -> IO (Maybe (String, String))
authorizeWithCredentials deribitClientId deribitClientSecret publicURL =  do
    request <- parseRequest $ publicURL <> "/auth"
    let
        headers = [("Content-Type", "application/json")]
        withHeaders = setRequestHeaders headers request
        queryParams = [
            ("client_id", Just (getClientId deribitClientId)),
            ("client_secret", Just (getClientSecret deribitClientSecret)),
            ("grant_type", Just "client_credentials")
          ]
        withAuthQuery = setQueryString queryParams withHeaders
    ibResponse <- httpBS withAuthQuery
    let maybeAccessToken = parseMaybe tokenParser =<< decodeStrict (getResponseBody ibResponse)
    case maybeAccessToken of
      Just tokenResponse -> return $ Just (accessToken tokenResponse, refreshToken tokenResponse)
      Nothing -> return Nothing

data TokenResponse = TokenResponse
  {
    accessToken :: String,
    refreshToken :: String
  } deriving Show

tokenParser :: Value -> Parser TokenResponse
tokenParser = withObject "AccessTokenResponse" $ \obj -> do
  resultObj <- obj .: "result"
  TokenResponse
    <$> resultObj .: "access_token"
    <*> resultObj .: "refresh_token"

data DeribitReportResult where
  DeribitReportResult :: {message :: String} -> DeribitReportResult
  deriving Generic
instance ToJSON DeribitReportResult

handler :: Value -> IO DeribitReportResult
handler _ = do
            deribitClientId <- lookupEnv "DERIBIT_CLIENT_ID"
            deribitClientSecret <- lookupEnv "DERIBIT_CLIENT_SECRET"
            case (deribitClientId, deribitClientSecret) of
                (Just deribitClientId', Just deribitClientSecret') -> do
                  credentials <- authorizeWithCredentials (DeribitClientId (BSC.pack deribitClientId')) (DeribitClientSecret (BSC.pack deribitClientSecret')) publicURL
                  case credentials of
                    Nothing ->  return $ DeribitReportResult { message = "authentication failed" }
                    Just (accessToken, refreshToken) -> return $ DeribitReportResult { message = "successfully authentified" }
                _ -> return $ DeribitReportResult { message = "error: environment variables DERIBIT_CLIENT_ID and DERIBIT_CLIENT_SECRET are required" }

    where
        hostName = "www.deribit.com"
        deribitEndpointV2 = "https://" ++ hostName ++ "/api/v2"
        publicURL = deribitEndpointV2 <> "/public"
        privateURL = deribitEndpointV2 <> "/private"


handlers :: Value -> Handler DeribitReportResult
handlers = liftIO . handler

type Api =
  "endpoint"
    :> ReqBody '[JSON] Value
    :> Post '[JSON] DeribitReportResult

app :: Application
app = serve (Proxy :: Proxy Api) handlers
