{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module GPGDecrypt (
    handleDecrypt, DecryptionRequest(..)
) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified System.Log.FastLogger as LOG

import qualified AWSEvent (RecordSet(..), Record(..))
import qualified Helper
import Control.Monad.IO.Class (liftIO)

data DecryptionResponse = DecryptionResponse T.Text | DecryptionError T.Text
    deriving (Show, Generic)
instance A.ToJSON DecryptionResponse

data DecryptionRequest = DecryptionRequest {
    objectPath :: T.Text,
    bucketName :: T.Text,
    pgpKey :: T.Text,
    pgpPassKey :: T.Text,
    decryptedObjectPath :: T.Text
} deriving (Show, Generic)
instance A.ToJSON DecryptionRequest
instance A.FromJSON DecryptionRequest

data PostEnvelop = Record
    { contents      :: DecryptionRequest
    , tag           :: T.Text
    } deriving (Show, Generic)

instance A.FromJSON PostEnvelop
instance A.ToJSON PostEnvelop

processRecord :: AWSEvent.Record -> IO DecryptionResponse
processRecord record = case (A.eitherDecode ((BSC.pack . T.unpack) (AWSEvent.body record)) :: Either String PostEnvelop) of
    Left msg -> return $ DecryptionError $ "failed to parse record: " <> T.pack msg
    Right body -> do
        loggerSet <- LOG.newStderrLoggerSet LOG.defaultBufSize
        let
            sourcePath = objectPath (contents body)
            bucket = bucketName (contents body)
            pKey = pgpKey (contents body)
            ppKey = pgpPassKey (contents body)
            targetPath = decryptedObjectPath (contents body)
        logMessage loggerSet $ T.pack "loading encrypted object: " <> T.pack (show sourcePath) <> " in bucket " <> T.pack (show bucket) 
        encrypted <- Helper.loadContentFromS3 bucket sourcePath

        logMessage loggerSet $ T.pack "saving decrypted object: " <> T.pack (show targetPath)
        Helper.writeToS3 bucket targetPath encrypted "application/octet-stream"
        logMessage loggerSet $ T.pack "not yet implemented: " <> T.pack (show body)
        return $ DecryptionError $ "not yet implemented: " <> T.pack (show body)

handleDecrypt :: A.Value -> IO [DecryptionResponse]
handleDecrypt sqsEvent = do
    loggerSet <- LOG.newStderrLoggerSet LOG.defaultBufSize
    logMessage loggerSet $ T.pack "processing event: " <> T.pack (show sqsEvent)
    case A.decode (A.encode sqsEvent) :: Maybe AWSEvent.RecordSet of
        Just (AWSEvent.RecordSet records) -> mapM processRecord records
        Nothing -> do
            logMessage loggerSet $ T.pack "failed to parse input event: " <> T.pack (show sqsEvent)
            return [DecryptionError $ "failed to parse input event" <> T.pack (show sqsEvent)]

logMessage :: LOG.LoggerSet -> T.Text -> IO ()
logMessage loggerSet msg = do
    LOG.pushLogStrLn loggerSet . LOG.toLogStr $ msg
