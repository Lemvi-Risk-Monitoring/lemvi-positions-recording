{-# LANGUAGE GADTs #-}

module Main (main) where

import AWS.Lambda.Runtime (mRuntime)
import qualified Network.Wai.Handler.Hal as WaiHandler

import qualified DeribitReports

main :: IO ()
main = mRuntime $ WaiHandler.run DeribitReports.app
