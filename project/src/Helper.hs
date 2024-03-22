module Helper (toCamel, toSnake, today, formatDate, writeToS3, loadFromS3, loadContentFromS3) where

import qualified Data.Functor as F
import qualified Data.Text as T

import GHC.Unicode ( toUpper, isUpper )
import Data.List (foldl')
import Data.Char (toLower)
import Data.Time.Clock ( getCurrentTime, UTCTime (utctDay) )
import Data.Time.Calendar ( toGregorian )
import Text.Printf (printf)
import Amazonka.S3.PutObject (putObject_contentType)
import Amazonka.S3.GetObject (getObjectResponse_body)

import qualified Data.ByteString.Char8 as BSC
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import qualified Control.Lens as CL
import qualified Data.Conduit.Binary as CB

toCamel :: String -> String
toCamel "" = ""
toCamel word = foldl' camelize [head word] $ zip (init word) (tail word)
    where
        camelize :: String -> (Char, Char) -> String
        camelize acc (prev, cur) = case (prev, cur) of
            ('_', _) -> acc ++ [toUpper cur]
            (_, '_') -> acc
            _ -> acc ++ [cur]

toSnake :: String -> String
toSnake "" = ""
toSnake word = foldl' snakize [] word
    where
        snakize :: String -> Char -> String
        snakize acc cur = if isUpper cur then acc ++ ['_'] ++ [toLower cur] else acc ++ [cur]

today :: IO (Integer, Int, Int) -- :: (year, month, day)
today = getCurrentTime F.<&> (toGregorian . utctDay)

-- Function to format a tuple (year, month, day) as Text
formatDate :: String -> (Integer, Int, Int) -> String
formatDate sep (year, month, day) = show year ++ sep ++ printf "%02d" month ++ sep ++ printf "%02d" day

writeToS3 :: T.Text -> T.Text -> BSC.ByteString -> T.Text -> IO ()
writeToS3 bucket filename content objectType = do
  env <- AWS.newEnv AWS.discover
  let
    request = S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey filename) (AWS.toBody content) CL.& putObject_contentType CL.?~ objectType
  _ <- AWS.runResourceT $ AWS.send env request
  return ()

loadFromS3 :: T.Text -> T.Text -> IO S3.GetObjectResponse
loadFromS3 bucket filename = do
  env <- AWS.newEnv AWS.discover
  let
    request = S3.newGetObject (S3.BucketName bucket) (S3.ObjectKey filename)
  AWS.runResourceT $ AWS.send env request

loadContentFromS3 :: T.Text -> T.Text -> IO BSC.ByteString
loadContentFromS3 bucket filename = do
  response <- loadFromS3 bucket filename
  let body = CL.view getObjectResponse_body response :: AWS.ResponseBody
  content <- AWS.runResourceT $ AWS.sinkBody body CB.sinkLbs
  return $ BSC.toStrict content
