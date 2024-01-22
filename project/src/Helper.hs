module Helper (toCamel, toSnake) where

import GHC.Unicode ( toUpper, isUpper )
import Data.List (foldl')
import Data.Char (toLower)


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
