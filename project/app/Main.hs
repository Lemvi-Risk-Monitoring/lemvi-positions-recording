{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main (main) where

import Aws.Lambda
import qualified Data.Text as T
import qualified Lib


main :: IO ()
main =
  runLambdaHaskellRuntime
    defaultDispatcherOptions
    (pure ())
    id
    (addStandaloneLambdaHandler (HandlerName (T.pack "handler")) Lib.handler)
