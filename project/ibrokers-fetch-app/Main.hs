{-# LANGUAGE GADTs #-}

module Main (main) where

import AWS.Lambda.Runtime (mRuntime)

import qualified IBrokersReports

main :: IO ()
main = mRuntime IBrokersReports.handleFetch
