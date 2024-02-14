module Main (main) where

import Test.Hspec

import qualified HelperSpec

main :: IO ()
main = hspec $ do
  describe "Helper tests" HelperSpec.spec
