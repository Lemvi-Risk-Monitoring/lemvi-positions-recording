{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aws.Lambda
import qualified Lib

main :: IO ()
main =
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id $ do
      -- You could also register multiple handlers
      addStandaloneLambdaHandler "handler" Lib.handler
