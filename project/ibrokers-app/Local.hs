{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp

import qualified IBrokersReports

main :: IO ()
main = Warp.run 8080 IBrokersReports.app 
