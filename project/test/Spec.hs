module Main (main) where

import Test.Hspec

import qualified HelperSpec
import qualified AWSEventDecoderSpec

main :: IO ()
main = hspec $ do
  describe "Helper tests" HelperSpec.spec
  describe "AWSEvent tests" AWSEventDecoderSpec.spec
