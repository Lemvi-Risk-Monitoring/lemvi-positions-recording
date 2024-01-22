{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where


import Data.Aeson.Types (Value)

import DeribitReports ( DeribitReportResult, handler )
import Servant (Proxy(Proxy), (:>), JSON, Post, ReqBody)
import Servant.Server (serve)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)

type API = "endpoint" :> ReqBody '[JSON] Value :> Post '[JSON] DeribitReportResult

main :: IO ()
main = run 8080 $ serve (Proxy :: Proxy API) $ liftIO . handler
