{-# LANGUAGE OverloadedStrings #-}

module DeribitReports (authorizeWithCredentials) where

import Network.HTTP.Simple (parseRequest, httpJSONEither, getResponseBody, httpBS)
import Data.ByteString (pack, unpack)

-- def authorize_with_credentials(client_id: str, client_secret: str) -> Tuple[Dict[str, str], str]:
--     print(f"connecting with {client_id}")
--     headers = {"Content-Type": "application/json"}
--     auth_params = {
--         "client_id": client_id,
--         "client_secret": client_secret,
--         "grant_type": "client_credentials"
--     }
--     authentication = requests.get(f"{public_url}/auth", params=auth_params, headers=headers).json()
--     headers["Authorization"] = f"Bearer {authentication['result']['access_token']}"
--     refresh_token = authentication['result']['refresh_token']
--     return headers, refresh_token

import qualified Data.ByteString.Char8 as BS


authorizeWithCredentials :: String -> String -> String -> IO (String, String)
authorizeWithCredentials clientId clientSecret publicURL =  do
    request <- parseRequest $ publicURL ++ "/auth"
    ibResponse <- httpBS request
    return (BS.unpack (getResponseBody ibResponse), "")
