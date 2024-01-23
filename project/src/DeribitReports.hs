{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module DeribitReports (app) where

import qualified Data.HashMap.Lazy as DH
import qualified Data.ByteString.Char8 as BSC

import GHC.Generics       (Generic)
import Network.HTTP.Simple (parseRequest, getResponseBody, httpBS, setRequestHeaders)
import Data.Aeson ( Value, decodeStrict, parseJSON, ToJSON )
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
    let result = decodeStrict (getResponseBody ibResponse) :: Maybe Value
    print ("result " ++ show result)
    return (extractToken "access_token" result, extractToken "refresh_token" result)

extractToken :: String -> Maybe Value -> Maybe String
extractToken field (Just mVal) = do
    obj <- parseMaybe (parseJSON :: Value -> Parser (DH.HashMap String Value)) mVal
    result <- DH.lookup "result" obj >>= parseMaybe (parseJSON :: Value -> Parser (DH.HashMap String String))
    DH.lookup field result
extractToken _ Nothing = Nothing

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
    :> ReqBody '[JSON] Value -- should be in query string !!!
    :> Post '[JSON] DeribitReportResult

app :: Application
app = serve (Proxy :: Proxy Api) handlers
