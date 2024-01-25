{-# LANGUAGE GADTs #-}

module Main (main) where

import AWS.Lambda.Runtime (mRuntime)

import qualified DeribitReports

main :: IO ()
main = mRuntime DeribitReports.handler
