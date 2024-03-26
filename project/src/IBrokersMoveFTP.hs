{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module IBrokersMoveFTP (
    handleMoveFTP,
    MoveFTPResponse
) where


import qualified Data.ByteString.Char8 as BS


import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import System.Environment (lookupEnv)
import qualified Network.Curl.Easy as NCE
import qualified System.Log.FastLogger as LOG

import qualified Helper
import qualified Network.Curl as NC
import qualified Data.IORef as IOR
import qualified Data.List as L
import qualified PostSQS
import GPGDecrypt ( DecryptionRequest(..) )

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as LT

data MoveFTPResult = MoveFTPResult { message :: T.Text } deriving (Generic, Show)
instance A.ToJSON MoveFTPResult
instance A.FromJSON MoveFTPResult

data MoveFTPResponse = MoveFTPResponse MoveFTPResult | MoveFTPError T.Text
  deriving (Generic, Show)
instance A.ToJSON MoveFTPResponse
instance A.FromJSON MoveFTPResponse

data ConfigMoveFTP = ConfigMoveFTP {
    ibPGPPrivateKeyPath :: String,
    ibPGPPassKey :: String,
    ibFTPServerName :: String,
    ibFTPUsername :: String,
    ibFTPPassword :: String,
    ibPositionsBucket :: String,
    decryptionQueue :: String
} deriving (Show)

getMoveFTPConfig :: IO (Maybe ConfigMoveFTP)
getMoveFTPConfig = do
    maybePpkp <- lookupEnv "IB_PGP_PRIVATE_KEY_PATH"
    maybePpk <- lookupEnv "IB_PGP_PASS_KEY"
    maybeFTPSrv <- lookupEnv "IB_FTP_SERVER"
    maybeFTPUser <- lookupEnv "IB_FTP_USERNAME"
    maybeFTPPwd <- lookupEnv "IB_FTP_PASSWORD"
    maybeBucket <- lookupEnv "IBROKERS_BUCKET_POSITIONS"
    maybeDecryptionQueue <- lookupEnv "DECRYPTION_QUEUE_URL"
    case (maybePpkp, maybePpk, maybeFTPSrv, maybeFTPUser, maybeFTPPwd, maybeBucket, maybeDecryptionQueue) of
        (Just ppkp, Just ppk, Just ftpSrv, Just ftpUser, Just ftpPwd, Just bucket, Just decryptQ) ->  return $ Just ConfigMoveFTP {
                ibPGPPrivateKeyPath = ppkp,
                ibPGPPassKey = ppk,
                ibFTPServerName  = ftpSrv,
                ibFTPUsername = ftpUser,
                ibFTPPassword = ftpPwd,
                ibPositionsBucket = bucket,
                decryptionQueue = decryptQ
            }
        _ -> return Nothing

handleMoveFTP :: A.Value -> IO MoveFTPResponse
handleMoveFTP _ = do
    loggerSet <- LOG.newStderrLoggerSet LOG.defaultBufSize
    config <- getMoveFTPConfig
    logMessage loggerSet $ "using config: " <> T.pack (show config)
    case config of
        Just cfg -> do
            curl <- NCE.initialize
            dirListRef <- IOR.newIORef []
            _ <- NCE.setopt curl (NC.CurlURL ("ftp://" <> ibFTPServerName cfg))
            _ <- NCE.setopt curl (NC.CurlPort 21)
            _ <- NCE.setopt curl (NC.CurlUserPwd (ibFTPUsername cfg <> ":" <> ibFTPPassword cfg))
            _ <- NCE.setopt curl (NC.CurlVerbose True)
            _ <- NCE.setopt curl (NC.CurlCustomRequest "LIST /outgoing")
            _ <- NCE.setopt curl (NC.CurlWriteFunction (NC.gatherOutput dirListRef))
            result <- NCE.perform curl
            case result of
                NC.CurlOK -> do
                    response <- IOR.readIORef dirListRef
                    let
                        extractFileNames :: String -> [String]
                        extractFileNames = map (last . words) . lines
                        fileList = (L.sort . extractFileNames . head) response

                    mapM_ (retrieveFile curl cfg) (filter endsWithSixNumbers fileList)
                    return $ MoveFTPResponse MoveFTPResult {message = T.pack (head response)}
                _ -> return $ MoveFTPError $ "failed to retrieve files from FTP server: " <> T.pack (show result)

        _ -> return $ MoveFTPError $ "unable to parse config: " <> T.pack (show config)

retrieveFile :: NC.Curl -> ConfigMoveFTP -> String -> IO ()
retrieveFile curl config fileName = do

    loggerSet <- LOG.newStderrLoggerSet LOG.defaultBufSize
    logMessage loggerSet $ "retrieving file " <> T.pack fileName

    let
        bucket = T.pack (ibPositionsBucket config)
        privateKeyPath = T.pack (ibPGPPrivateKeyPath config)
        ppk = T.pack (ibPGPPrivateKeyPath config)
        queueURL = T.pack (decryptionQueue config)
        (resourceBucket, pgpKeyObject) = case T.splitOn "/" privateKeyPath of
            [item1, item2] -> (item1, item2)
            _ -> error $ "failed to parse encryption key " <> T.unpack privateKeyPath

    fileContentRef <- IOR.newIORef []
    _ <- NCE.setopt curl (NC.CurlCustomRequest ("RETR " <> "/outgoing/" <> fileName))
    _ <- NCE.setopt curl (NC.CurlWriteFunction (NC.gatherOutput fileContentRef))
    downloadResult <- NCE.perform curl
    case downloadResult of
        NC.CurlOK -> do
            content <- IOR.readIORef fileContentRef
            let
                targetDate = (reverse . take 8 . drop 8 . reverse) fileName
                targetYear = take 4 targetDate
                targetMonth = (take 2 . drop 4) targetDate
                targetObjectPath =  T.pack $ targetYear <> "/" <> targetMonth <> "/" <> fileName
                targetObjectName = T.pack "encrypted/" <> targetObjectPath
            logMessage loggerSet $ "storing file as s3:" <> bucket <> "/" <> targetObjectName
            Helper.writeToS3 bucket targetObjectName (BS.pack (head content)) "application/octet-stream"
            Helper.writeToS3 bucket (T.pack "encrypted/latest.gpg") (BS.pack (head content)) "application/octet-stream"
            pgpk <- Helper.loadContentFromS3 resourceBucket pgpKeyObject
            let decryptionRequest = DecryptionRequest { objectPath=targetObjectName, bucketName=bucket, pgpKey=TE.decodeUtf8 pgpk, pgpPassKey=ppk, decryptedObjectPath=targetObjectPath }
            PostSQS.sendMessageQueueURL queueURL ((LT.toStrict . encodeToLazyText) decryptionRequest)
            _ <- NCE.setopt curl (NC.CurlCustomRequest ("DELE " <> "/outgoing/" <> fileName))
            deleteResult <- NCE.perform curl
            case deleteResult of
                NC.CurlOK -> logMessage loggerSet $ "successfully processed file " <> T.pack fileName
                _ -> logMessage loggerSet $ "failed to delete processed file " <> T.pack fileName
        _ -> logMessage loggerSet $ "failed to process file " <> T.pack fileName

endsWithSixNumbers :: String -> Bool
endsWithSixNumbers fileName =
    case reverse fileName of
        ('p':'g':'p':'.':'l':'m':'x':'.':rest) | length rest >= 8 && all (`elem` ['0'..'9']) (take 8 rest) -> True
        _ -> False

logMessage :: LOG.LoggerSet -> T.Text -> IO ()
logMessage loggerSet msg = do
    LOG.pushLogStrLn loggerSet . LOG.toLogStr $ msg
