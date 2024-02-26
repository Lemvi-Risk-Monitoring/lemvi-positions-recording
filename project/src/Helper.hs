module Helper (toCamel, toSnake, today, formatDate, writeToS3) where

import qualified Data.Functor as F
import qualified Data.Text as T

import GHC.Unicode ( toUpper, isUpper )
import Data.List (foldl')
import Data.Char (toLower)
import Data.Time.Clock ( getCurrentTime, UTCTime (utctDay) )
import Data.Time.Calendar ( toGregorian )
import Text.Printf (printf)
import Amazonka.S3.PutObject (putObject_contentType)

import qualified Data.ByteString.Char8 as BSC
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import qualified Control.Lens as CL

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
writeToS3 bucket filename json objectType = do
  env <- AWS.newEnv AWS.discover
  let
    request = S3.newPutObject (S3.BucketName bucket) (S3.ObjectKey filename) (AWS.toBody json) CL.& putObject_contentType CL.?~ objectType
  _ <- AWS.runResourceT $ AWS.send env request
  return ()
