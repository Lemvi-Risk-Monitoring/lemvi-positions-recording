{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import qualified Data.Aeson as A

import qualified GPGDecrypt

main :: IO ()
main = do
  result <- GPGDecrypt.handleDecrypt (A.Null :: A.Value)
  print result
