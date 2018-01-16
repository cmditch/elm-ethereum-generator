{-# LANGUAGE OverloadedStrings #-}

module Utils (
    toLowerFirst,
    toUpperFirst,
    minify,
    indents,
    (|>)
    ) where

import           Data.Char      (isSeparator, toLower, toUpper)
import           Data.Int       (Int64)
import           Data.Monoid    ((<>))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Prelude



(|>) x f = f x

-- | Lower first char of string
toLowerFirst :: String -> String
toLowerFirst []       = []
toLowerFirst (x : xs) = toLower x : xs


-- | Upper first char of string
toUpperFirst :: String -> String
toUpperFirst []       = []
toUpperFirst (x : xs) = toUpper x : xs


minify :: Text ->  Text
minify =
    Text.filter (not . (\c -> c == '\n' || c == '\r' || c == '\t' || isSeparator c))


-- | Number of 4-space-tabs to indent line
indents :: Int64 -> Text -> Text
indents n t =
    Text.replicate (n * 4) " " <> t
