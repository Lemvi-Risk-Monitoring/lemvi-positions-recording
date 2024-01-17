module Main (main) where

import qualified Services
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = Warp.runEnv 8080 Services.app
