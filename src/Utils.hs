{-# LANGUAGE OverloadedStrings #-}

module Utils (
    (|>),
    toLowerFirst,
    toUpperFirst,
    textLowerFirst,
    textUpperFirst,
    newLine,
    minify,
    indent,
    paramAlphabet,
    getFileName,
    cleanPrependingChars,
    tshow
    ) where

import           Data.Char      (isAlphaNum, isSeparator, toLower, toUpper)
import           Data.Int       (Int64)
import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Data.Text      as Text
import           Prelude

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | Pipeline operator, a la Elm.
(|>) :: a -> (a -> b) -> b
x |> f = f x


-- | Lower first char of string
toLowerFirst :: String -> String
toLowerFirst []       = []
toLowerFirst (x : xs) = toLower x : xs


-- | Lower first char of string
toUpperFirst :: String -> String
toUpperFirst []       = []
toUpperFirst (x : xs) = toUpper x : xs


-- | Lower first char of string
textLowerFirst :: Text -> Text
textLowerFirst t = (Text.singleton . toLower $ Text.head t) <> Text.tail t

-- | Lower first char of string
textUpperFirst :: Text -> Text
textUpperFirst t = (Text.singleton . toUpper $ Text.head t) <> Text.tail t

newLine :: Text -> Text
newLine = (<> "\n")

-- | Remove all spaces, tabs, and newlines
minify :: Text ->  Text
minify =
    Text.filter (not . (\c -> c == '\n' || c == '\r' || c == '\t' || isSeparator c))


-- | Number of 4-space-tabs to indent line
indent :: Int -> Text -> Text
indent n t =
    Text.replicate (n * 4) " " <> t


-- | Used to generate parameter names
-- | "*!*" is 17th letter to cause Elm compiler to fail if function inputs > 16
-- | Due to stack limitations on the EVM
paramAlphabet :: String
paramAlphabet =
    ['a' .. 'p'] <> "*!*"

-- | "~/some/path/ERC20.elm" to "ERC20"
getFileName :: FilePath -> Text
getFileName name = removeExtension $ splitPaths $ Text.pack name
    where
        splitPaths = last . Text.splitOn "/"
        removeExtension = head . Text.splitOn "."


-- | Removes non-alphanumeric prepending characters.
-- | "_someVar" to "someVar"
cleanPrependingChars :: Text -> Text
cleanPrependingChars t =
    let
        (x:xs) = Text.unpack t
    in
        if isAlphaNum x
        then t
        else cleanPrependingChars $ Text.pack xs
