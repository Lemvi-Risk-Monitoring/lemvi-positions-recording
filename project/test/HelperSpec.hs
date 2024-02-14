module HelperSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec )

import Helper (toCamel, toSnake)

spec :: Spec
spec = do
  describe "Camelization" $ do

    it "camelizes empty string" $ do
      toCamel "" `shouldBe` ""

    it "camelizes simple names" $ do
      toCamel "test_me_simple" `shouldBe` "testMeSimple"

  describe "Words to snake style" $ do
    it "translates word to snake style" $ do
      toSnake "testMeSimple" `shouldBe` "test_me_simple"
