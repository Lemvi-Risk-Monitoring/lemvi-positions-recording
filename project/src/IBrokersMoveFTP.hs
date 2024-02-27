{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module IBrokersMoveFTP (
    handleMoveFTP,
    MoveFTPResponse
) where

import Network.HTTP.Simple ( parseRequest, getResponseBody, httpBS, setRequestResponseTimeout )

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSC

import Text.XML.Light
    ( unqual,
      findElements,
      Attr(Attr),
      Element(elAttribs),
      QName(qName), parseXMLDoc, strContent, findElement, findAttr )

import Data.Map.Strict (fromList)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Control.Exception (throw, Exception)
import Data.Data (Typeable)
import Network.HTTP.Conduit ( responseTimeoutMicro )
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import Data.Aeson.Types (parseMaybe)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import qualified System.Log.FastLogger as LOG

import qualified PostSQS
import qualified Helper
import qualified AWSEvent (RecordSet(..), Record(..))


data MoveFTPResult = MoveFTPResult { message :: T.Text } deriving Generic
instance A.ToJSON MoveFTPResult

data MoveFTPResponse = MoveFTPResponse MoveFTPResult | MoveFTPError T.Text
  deriving Generic
instance A.ToJSON MoveFTPResponse

data ReportFetchResponse = ReportFetchResponse T.Text | ReportFetchError T.Text
  deriving Generic
instance A.ToJSON ReportFetchResponse

data ConfigMoveFTP = ConfigMoveFTP {
    ibPGPPrivateKeyPath :: String,
    ibPGPPassKey :: String,
    ibFTPServerName :: String,
    ibFTPUsername :: String,
    ibFTPPassword :: String,
    ibPositionsBucket :: String
}

        --       "IB_PGP_PRIVATE_KEY_PATH": "${aws_s3_bucket.lambda_resources_bucket.bucket}/private-ibrokers-reporting.pgp",
        --   "IB_PGP_PASS_KEY": var.ib_pgp_pass_key,
        --   "IB_FTP_SERVER": var.ib_ftp_server,
        --   "IB_FTP_USERNAME": var.ib_ftp_username,
        --   "IB_FTP_PASSWORD": var.ib_ftp_password,
        --   "IBROKERS_BUCKET_POSITIONS": aws_s3_bucket.ibrokers_bucket.bucket

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
    case config of
        Just cfg -> return $ MoveFTPError "required env variables: IB_FLEX_REPORT_TOKEN, IBROKERS_BUCKET_POSITIONS"
        _ -> return $ MoveFTPError "required env variables: IB_FLEX_REPORT_TOKEN, IBROKERS_BUCKET_POSITIONS"


logMessage :: LOG.LoggerSet -> T.Text -> IO ()
logMessage loggerSet msg = do
    LOG.pushLogStrLn loggerSet . LOG.toLogStr $ msg
