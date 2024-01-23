module Main (main) where

import AWS.Lambda.Runtime (mRuntime)
import qualified Network.Wai.Handler.Hal as WaiHandler

import qualified Echo

main :: IO ()
main = mRuntime $ WaiHandler.run Echo.app
