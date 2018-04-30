{-# LANGUAGE OverloadedStrings #-}

module Generator.Templates
    ( moduleName
    , imports
    , callBuilder
    , logFilterBuilder
    ) where

import           Data.Monoid    ((<>))
import           Data.Text.Lazy (Text)


-- | Declare module/contract name
moduleName :: Text -> [Text]
moduleName name =
    ["module " <> name <> " exposing (..)\n"]


-- | Elm imports
imports :: [Text]
imports =
    [ "import BigInt exposing (BigInt)"
    , "import Json.Decode as Decode exposing (Decoder)"
    , "import Json.Decode.Pipeline exposing (custom, decode)"
    , "import Web3.Types exposing (..)"
    , "import Web3.Eth.Types exposing (..)"
    , "import Web3.Evm.Decode exposing (..)"
    , "import Web3.Evm.Encode as Evm exposing (..)"
    , "import Web3.Utils exposing (keccak256)"
    , ""
    ]


-- | Return type for contract functions
callBuilder :: Text -> Text -> Text -> [Text]
callBuilder sig encodings decoder =
    [ "{ to = Just contractAddress"
    , ", from = Nothing"
    , ", gas = Nothing"
    , ", gasPrice = Nothing"
    , ", value = Nothing"
    , ", data = Just <| encodeData " <> sig <> " " <> encodings
    , ", nonce = Nothing"
    , ", decoder = " <> decoder
    , "}"
    ]

logFilterBuilder :: Text -> [Text]
logFilterBuilder topics =
   [ "{ fromBlock = LatestBlock"
   , ", toBlock = LatestBlock"
   , ", address = contractAddress"
   , ", topics = " <> topics
   , "}"
   ]
