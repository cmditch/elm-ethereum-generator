module Internal (
    toLowerFirst,
    toUpperFirst
    ) where

import Data.Char (toLower, toUpper)

-- | Lower first char of string
toLowerFirst :: String -> String
toLowerFirst [] = []
toLowerFirst (x : xs) = toLower x : xs

-- | Upper first char of string
toUpperFirst :: String -> String
toUpperFirst [] = []
toUpperFirst (x : xs) = toUpper x : xs
