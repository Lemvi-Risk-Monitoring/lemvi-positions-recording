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
import qualified IBrokersReports

type Api =
  "local"
    S.:> S.ReqBody '[S.JSON] A.Value
    S.:> S.Post '[S.JSON] IBrokersReports.ReportRequestResponse

handlers :: A.Value -> S.Handler IBrokersReports.ReportRequestResponse
handlers = liftIO . IBrokersReports.handleRequest

app :: SRV.Application
app = S.serve (Proxy :: Proxy Api) handlers

main :: IO ()
main = Warp.run 8080 app 
