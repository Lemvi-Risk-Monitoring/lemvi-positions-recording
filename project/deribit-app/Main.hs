{-# LANGUAGE GADTs #-}

module Main (main) where

import AWS.Lambda.Runtime (mRuntime)
import DeribitReports (app)
import qualified Network.Wai.Handler.Hal as WaiHandler


main :: IO ()
main = mRuntime $ WaiHandler.run DeribitReports.app
