{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module DeribitReports (app) where

import qualified Data.ByteString.Char8 as BSC

import GHC.Generics       (Generic)
import Network.HTTP.Simple (parseRequest, getResponseBody, httpBS, setRequestHeaders)
import Data.Aeson ( Value, decodeStrict, ToJSON, withObject, (.:) )
import Data.Aeson.Types ( parseMaybe, Parser )
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Servant (Proxy(Proxy), (:>), JSON, Post, ReqBody)
import Servant.Server (Application, serve, Handler)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client (setQueryString)


authorizeWithCredentials :: String -> String -> String -> IO (Maybe String, Maybe String)
authorizeWithCredentials deribitClientId deribitClientSecret publicURL =  do
    request <- parseRequest $ publicURL ++ "/auth"
    let
        headers = [("Content-Type", "application/json")]
        withHeaders = setRequestHeaders headers request
        queryParams = [
            ("client_id", Just (BSC.pack deribitClientId)),
            ("client_secret", Just (BSC.pack deribitClientSecret)),
            ("grant_type", Just "client_credentials")
          ]
        withAuthQuery = setQueryString queryParams withHeaders
    ibResponse <- httpBS withAuthQuery
    let maybeAccessToken = parseMaybe tokenParser =<< decodeStrict (getResponseBody ibResponse)
    case maybeAccessToken of
      Just tokenResponse -> do
        return (Just (access_token tokenResponse), Just (refresh_token tokenResponse))
      Nothing -> return (Nothing, Nothing)

data TokenResponse = TokenResponse
  {
    access_token :: String,
    refresh_token :: String
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
                  (content, _) <- authorizeWithCredentials deribitClientId' deribitClientSecret' publicURL
                  return $ DeribitReportResult { message = fromMaybe "failed" content }
                _ -> return $ DeribitReportResult { message = "error" }

    where
        hostName = "www.deribit.com"
        deribitEndpointV2 = "https://" ++ hostName ++ "/api/v2"
        publicURL = deribitEndpointV2 ++ "/public"
        privateURL = deribitEndpointV2 ++ "/private"


handlers :: Value -> Handler DeribitReportResult
handlers = liftIO . handler

type Api =
  "endpoint"
    :> ReqBody '[JSON] Value
    :> Post '[JSON] DeribitReportResult

app :: Application
app = serve (Proxy :: Proxy Api) handlers
