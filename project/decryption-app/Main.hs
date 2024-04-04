{-# LANGUAGE GADTs #-}

module Main (main) where

import AWS.Lambda.Runtime (mRuntime)

import qualified GPGDecrypt

main :: IO ()
main = mRuntime GPGDecrypt.handleDecrypt
