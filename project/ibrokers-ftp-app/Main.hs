{-# LANGUAGE GADTs #-}

module Main (main) where

import AWS.Lambda.Runtime (mRuntime)

import qualified IBrokersMoveFTP

main :: IO ()
main = mRuntime IBrokersMoveFTP.handleMoveFTP
