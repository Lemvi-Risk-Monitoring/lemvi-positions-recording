module Main (main) where

import AWS.Lambda.Runtime (mRuntime)
import qualified Lib
import qualified Network.Wai.Handler.Hal as WaiHandler

main :: IO ()
main = mRuntime $ WaiHandler.run Lib.app
