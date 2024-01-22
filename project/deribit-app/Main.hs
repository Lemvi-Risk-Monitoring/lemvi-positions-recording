{-# LANGUAGE GADTs #-}

module Main (main) where

import AWS.Lambda.Runtime (mRuntime)
import DeribitReports (handler)


main :: IO ()
main = mRuntime handler
