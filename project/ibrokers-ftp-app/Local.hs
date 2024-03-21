{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import qualified Data.Aeson as A

import qualified IBrokersMoveFTP

main :: IO ()
main = do
  result <- IBrokersMoveFTP.handleMoveFTP (A.Null :: A.Value)
  print result
