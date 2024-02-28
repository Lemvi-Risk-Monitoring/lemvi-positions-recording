{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module IBrokersMoveFTP (
    handleMoveFTP,
    MoveFTPResponse
) where


import qualified Data.ByteString.Char8 as BS


import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import System.Environment (lookupEnv)
import qualified Network.Curl.Easy as NCE
import qualified System.Log.FastLogger as LOG

import qualified Helper
import qualified Network.Curl as NC
import qualified Data.IORef as IOR
import qualified Data.List as L


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
    ibPositionsBucket :: String
} deriving (Show)

getMoveFTPConfig :: IO (Maybe ConfigMoveFTP)
getMoveFTPConfig = do
    maybePpkp <- lookupEnv "IB_PGP_PRIVATE_KEY_PATH"
    maybePpk <- lookupEnv "IB_PGP_PASS_KEY"
    maybeFTPSrv <- lookupEnv "IB_FTP_SERVER"
    maybeFTPUser <- lookupEnv "IB_FTP_USERNAME"
    maybeFTPPwd <- lookupEnv "IB_FTP_PASSWORD"
    maybeBucket <- lookupEnv "IBROKERS_BUCKET_POSITIONS"
    case (maybePpkp, maybePpk, maybeFTPSrv, maybeFTPUser, maybeFTPPwd, maybeBucket) of
        (Just ppkp, Just ppk, Just ftpSrv, Just ftpUser, Just ftpPwd, Just bucket) ->  return $ Just ConfigMoveFTP {
                ibPGPPrivateKeyPath = ppkp,
                ibPGPPassKey = ppk,
                ibFTPServerName  = ftpSrv,
                ibFTPUsername = ftpUser,
                ibFTPPassword = ftpPwd,
                ibPositionsBucket = bucket
            }
        _ -> return Nothing

handleMoveFTP :: A.Value -> IO MoveFTPResponse
handleMoveFTP requestMoveFTP = do
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
            --_ <- NCE.setopt curl (NC.CurlVerbose True)
            _ <- NCE.setopt curl (NC.CurlQuote ["CWD outgoing"])
            _ <- NCE.setopt curl (NC.CurlCustomRequest "LIST")
            _ <- NCE.setopt curl (NC.CurlWriteFunction (NC.gatherOutput dirListRef))
            result <- NCE.perform curl
            case result of
                NC.CurlOK -> do
                    response <- IOR.readIORef dirListRef
                    let
                        extractFileNames :: String -> [String]
                        extractFileNames = map (last . words) . lines
                        fileList = (L.sort . extractFileNames . head) response

                    mapM_ (retrieveFile curl (T.pack (ibPositionsBucket cfg))) (filter endsWithSixNumbers fileList)
                    return $ MoveFTPResponse MoveFTPResult {message = T.pack (head response)}
                _ -> return $ MoveFTPError $ "failed to retrieve files from FTP server: " <> T.pack (show result)

        _ -> return $ MoveFTPError $ "unable to parse config: " <> T.pack (show config)

retrieveFile :: NC.Curl -> T.Text -> String -> IO ()
retrieveFile curl bucket fileName = do
    loggerSet <- LOG.newStderrLoggerSet LOG.defaultBufSize
    logMessage loggerSet $ "retrieving file " <> T.pack fileName
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
                targetObjectName = "encrypted/" <> targetYear <> "/" <> targetMonth <> "/" <> fileName
            logMessage loggerSet $ "storing file as s3:" <> bucket <> "/" <> T.pack targetObjectName
            Helper.writeToS3 bucket (T.pack targetObjectName) (BS.pack (head content)) "application/octet-stream"
            _ <- NCE.setopt curl (NC.CurlCustomRequest ("DEL /outgoing/" <> fileName))
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
