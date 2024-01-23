module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp

import qualified Echo

main :: IO ()
main = Warp.runEnv 8080 Echo.app
