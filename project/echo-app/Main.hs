module Main (main) where

import AWS.Lambda.Runtime (mRuntime)
import qualified Network.Wai.Handler.Hal as WaiHandler

import qualified Echo

import AWS.Lambda.Events.ApiGateway.ProxyRequest (ProxyRequest, NoAuthorizer)
import AWS.Lambda.Events.ApiGateway.ProxyResponse (ProxyResponse)

handler :: ProxyRequest NoAuthorizer -> IO ProxyResponse
handler = WaiHandler.run Echo.app

main :: IO ()
main = mRuntime handler
