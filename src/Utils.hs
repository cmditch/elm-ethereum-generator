{-# LANGUAGE OverloadedStrings #-}

module Utils (
    (|>),
    toLowerFirst,
    textLowerFirst,
    minify,
    indent,
    paramAlphabet
    ) where

import           Data.Char      (isSeparator, toLower, toUpper)
import           Data.Int       (Int64)
import           Data.Monoid    ((<>))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Prelude


-- | Pipeline operator, a la Elm.
(|>) :: a -> (a -> b) -> b
x |> f = f x


-- | Lower first char of string
toLowerFirst :: String -> String
toLowerFirst []       = []
toLowerFirst (x : xs) = toLower x : xs


-- | Lower first char of string
textLowerFirst :: Text -> Text
textLowerFirst t = (Text.singleton . toLower $ Text.head t) <> Text.tail t


-- | Remove all spaces, tabs, and newlines
minify :: Text ->  Text
minify =
    Text.filter (not . (\c -> c == '\n' || c == '\r' || c == '\t' || isSeparator c))


-- | Number of 4-space-tabs to indent line
indent :: Int64 -> Text -> Text
indent n t =
    Text.replicate (n * 4) " " <> t


-- | Used to generate parameter names
-- | "!" is 17th letter to cause Elm compiler to fail if function inputs > 16
-- | Due to stack limitations on the EVM
paramAlphabet :: String
paramAlphabet =
    ['a' .. 'p'] <> "!"
