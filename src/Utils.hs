{-# LANGUAGE OverloadedStrings #-}

module Utils (
    toLowerFirst,
    toUpperFirst,
    minify,
    typeCast,
    multiLineRecordType,
    singleLineRecordType
    ) where

import           Data.Char      (isSeparator, toLower, toUpper)
import           Data.Monoid    ((<>))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Prelude


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


-- | Convert Solidity type in ABI to elm-web3 type
typeCast :: Text -> Text
typeCast "string" = "String"
typeCast "address" = "Address"
typeCast tipe | Text.isPrefixOf "int" tipe = "BigInt"
              | Text.isPrefixOf "uint" tipe = "BigInt"
              | otherwise = tipe <> "-ERROR!"


{-
    { a : String
    , b : Int
    , c : Bool
    }
-}
multiLineRecordType :: [Text] -> Text
multiLineRecordType fields =
  "\n    { " <> Text.intercalate "\n    , " fields <> "\n    }"

-- | { a : String, b : Int }
singleLineRecordType :: [Text] -> Text
singleLineRecordType field =
  "{ " <> Text.intercalate ", " field <> " }"
