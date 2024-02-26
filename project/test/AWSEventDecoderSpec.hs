{-# LANGUAGE OverloadedStrings #-}
module AWSEventDecoderSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec, expectationFailure )
import Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified AWSEvent (RecordSet(..), Record(..))
import qualified IBrokersReports (CallbackBody(..), ReportRequestResult(..))


spec :: Spec
spec = do
  describe "Parsing AWS Event" $ do

    it "decodes the input json" $ do
      let
        jsonText = BSC.pack "{\"Records\":[{\"attributes\":{\"ApproximateFirstReceiveTimestamp\":\"1708936495747\",\"ApproximateReceiveCount\":\"1\",\"SenderId\":\"AROA4PO6CV2X2WFJWYRUV:test-ibrokers-request-lambda\",\"SentTimestamp\":\"1708936465747\"},\"awsRegion\":\"us-east-1\",\"body\":\"{\\\"contents\\\":{\\\"callbackURL\\\":\\\"https://gdcdyn.interactivebrokers.com/Universal/servlet/FlexStatementService.GetStatement\\\",\\\"countRetries\\\":5,\\\"referenceCode\\\":\\\"4455189702\\\"},\\\"tag\\\":\\\"ReportRequestResponse\\\"}\",\"eventSource\":\"aws:sqs\",\"eventSourceARN\":\"arn:aws:sqs:us-east-1:857848589999:test-ibrokers-reporting\",\"md5OfBody\":\"42864201b7d9668900249ccf5018cf6c\",\"messageAttributes\":{},\"messageId\":\"1c786e30-232c-40e8-a187-7f9a0950018b\",\"receiptHandle\":\"AQEBo72OOqOSdMe3zwW5WpG1qteGdF+PzBoHVwcuAO4sIrYQltm4UCYkYnBCdsp6yQvjw2Ws6HJYx/2GjT9/WF4Nb4uVYQawAn490ESxOZdN13RiLzPb/zjEk74mvB5NBnPXrGWkUMxa5On2tG6secZdAdLUOC0ZAGCkScVZAJwvoAdrWnK5QzdTWlC7dFuwaLrS5s0OX04mHD6MjAh7Nm+DYCtGIFzoBpmlaEcWJGgJMQjlXgxrcnX5S7gGpAbUhe609IkyC1CZTUG9BoLmiy0nDTRdLTYy5c8kOatwR0g+DmviYeqa3gAoO9hnHdhJ11/wi9sfBmfp2teaotxNHs1auEcerUi1BWIrb3BxaPU6CTh5xev8S72HkfE/1rAg1uPTnH4gZwu4faqW2KmIqzE9Lg==\"}]}"
        parsed = A.eitherDecode jsonText :: Either String AWSEvent.RecordSet
      case parsed of
        Left message -> expectationFailure $ "failed to parse JSON: " <> message
        Right rows -> do
          AWSEvent.awsRegion firstRow `shouldBe` T.pack "us-east-1"
          AWSEvent.body firstRow `shouldBe` T.pack "{\"contents\":{\"callbackURL\":\"https://gdcdyn.interactivebrokers.com/Universal/servlet/FlexStatementService.GetStatement\",\"countRetries\":5,\"referenceCode\":\"4455189702\"},\"tag\":\"ReportRequestResponse\"}"
          where
            firstRow = Prelude.head $ AWSEvent.records rows

    it "parses the body" $ do
      let
        body = BSC.pack "{\"contents\":{\"callbackURL\":\"https://gdcdyn.interactivebrokers.com/Universal/servlet/FlexStatementService.GetStatement\",\"countRetries\":5,\"referenceCode\":\"4455189702\"},\"tag\":\"ReportRequestResponse\"}"
        Right parsed = A.eitherDecode body :: Either String IBrokersReports.CallbackBody
      IBrokersReports.callbackURL (IBrokersReports.contents parsed) `shouldBe` "https://gdcdyn.interactivebrokers.com/Universal/servlet/FlexStatementService.GetStatement"
