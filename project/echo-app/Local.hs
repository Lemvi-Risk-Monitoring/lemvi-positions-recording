module Main (main) where

import qualified Echo
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.runEnv 8080 Echo.app
