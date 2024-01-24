module Helper (toCamel, toSnake, today, formatDate) where

import qualified Data.Functor as F
import qualified Data.Text as T

import GHC.Unicode ( toUpper, isUpper )
import Data.List (foldl')
import Data.Char (toLower)
import Data.Time.Clock
    ( getCurrentTime, UTCTime(UTCTime, utctDay) )
import Data.Time.Calendar ( fromGregorian, toGregorian )
import Data.Time (formatTime)
import Data.Time.Format.ISO8601 (formatShow)

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
formatDate sep (year, month, day) = show year ++ sep ++ show month ++ sep ++ show day
