module Internal (
    toLowerFirst,
    toUpperFirst,
    minify
    ) where

import           Data.Char      (isSeparator, toLower, toUpper)
import qualified Data.Text.Lazy as T

-- | Lower first char of string
toLowerFirst :: String -> String
toLowerFirst []       = []
toLowerFirst (x : xs) = toLower x : xs

-- | Upper first char of string
toUpperFirst :: String -> String
toUpperFirst []       = []
toUpperFirst (x : xs) = toUpper x : xs

minify :: T.Text -> T.Text
minify =
  T.filter (not . (\c -> c == '\n' || c == '\r' || c == '\t' || isSeparator c))
