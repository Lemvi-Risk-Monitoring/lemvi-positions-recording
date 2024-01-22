{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TemplateHaskell #-}

module DeribitReports (handler, DeribitReportResult) where

import GHC.Generics       (Generic)
import Network.HTTP.Simple (parseRequest, getResponseBody, httpBS, setRequestHeaders, setRequestBodyJSON)

import Data.Aeson.TH (deriveJSON, defaultOptions, Options(fieldLabelModifier))

import Helper (toSnake)
import Data.Aeson ( Value, decodeStrict, parseJSON, ToJSON, encode )
import qualified Data.HashMap.Lazy as DH
import Data.Aeson.Types (parseMaybe, Parser)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

data AuthParams = AuthParams {
    clientId :: String,
    clientSecret :: String,
    grantType :: String
    }
  deriving (Generic, Show)

$(deriveJSON defaultOptions {fieldLabelModifier = toSnake} ''AuthParams)

authorizeWithCredentials :: String -> String -> String -> IO (Maybe String, Maybe String)
authorizeWithCredentials deribitClientId deribitClientSecret publicURL =  do
    request <- parseRequest $ publicURL ++ "/auth"
    let
        headers = [("Content-Type", "application/json")]
        authParams = AuthParams { clientId=deribitClientId, clientSecret=deribitClientSecret, grantType="client_credentials"}
        withHeaders = setRequestHeaders headers request
        withAuthBody = setRequestBodyJSON authParams withHeaders
    ibResponse <- trace ("calling url " ++ show withAuthBody) trace ("calling url " ++ show (encode authParams)) httpBS withAuthBody
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
