module Helper (toCamel, toSnake, today, formatDate) where

import qualified Data.Functor as F

import GHC.Unicode ( toUpper, isUpper )
import Data.List (foldl')
import Data.Char (toLower)
import Data.Time.Clock ( getCurrentTime, UTCTime (utctDay) )
import Data.Time.Calendar ( toGregorian )
import Text.Printf (printf)

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
