{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.Aeson as A
import qualified Servant as S
import qualified Servant.Server as SRV

import Control.Monad.IO.Class (liftIO)
import Servant ( Proxy(Proxy) )

import qualified IBrokersMoveFTP

type Api =
  "local"
    S.:> S.ReqBody '[S.JSON] A.Value
    S.:> S.Post '[S.JSON] IBrokersMoveFTP.MoveFTPResponse

handlers :: A.Value -> S.Handler IBrokersMoveFTP.MoveFTPResponse
handlers = liftIO . IBrokersMoveFTP.handleMoveFTP

app :: SRV.Application
app = do
  S.serve (Proxy :: Proxy Api) handlers

main :: IO ()
main = do
  putStrLn "starting server listening on 8080..."
  Warp.run 8080 app 
