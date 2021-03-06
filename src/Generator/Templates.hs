{-# LANGUAGE OverloadedStrings #-}

module Generator.Templates
    ( moduleNameAndExports
    , encodeModuleName
    , decodeModuleName
    , imports
    , callBuilder
    , logFilterBuilder
    ) where

import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import           Data.Version   (showVersion)
import qualified Data.List
import qualified Data.Text      as Text
import qualified Utils          as U
import qualified Paths_elm_ethereum_generator


-- | Declare module/contract name
moduleNameAndExports :: Text -> [Text] -> [Text]
moduleNameAndExports name varNames =
    [ "module Contracts." <> name <> " exposing"
    , U.indent 1  ("( " <> x)
    ] <> (exportHelper <$> xs) <> [ U.indent 1 ")\n" ]
    where
        (x:xs) = Data.List.sort varNames
        exportHelper n = U.indent 1 (", " <> n)

encodeModuleName :: Text
encodeModuleName = "E"


decodeModuleName :: Text
decodeModuleName = "D"


-- | Elm imports
imports :: [Text]
imports =
    [ "import Eth.Abi.Decode as " <> decodeModuleName <> " exposing (abiDecode, andMap, data, toElmDecoder, topic)"
    , "import Eth.Abi.Encode as " <> encodeModuleName <> " exposing (Encoding(..), abiEncode)"
    , "import BigInt exposing (BigInt)"
    , "import Eth.Types exposing (..)"
    , "import Eth.Utils as U"
    , "import Json.Decode as Decode exposing (Decoder, succeed)"
    , "import Json.Decode.Pipeline exposing (custom)"
    , ""
    , ""
    , ""
    , "{-"
    , ""
    , "   This file was generated by https://github.com/cmditch/elm-ethereum-generator v" <> (Text.pack $ showVersion Paths_elm_ethereum_generator.version)
    , "   Compatible with elm-ethereum v4.0.0"
    , ""
    , "-}"
    ]


-- | Return type for contract functions
callBuilder :: Bool -> Text -> Text -> Text -> [Text]
callBuilder isDebug sig encodings decoder =
    let
        encodeDataFunc =
            if isDebug then
                " " <> encodeModuleName <> ".functionCallWithDebug "
            else
                " " <> encodeModuleName <> ".functionCall "
    in
    [ "{ to = Just contractAddress"
    , ", from = Nothing"
    , ", gas = Nothing"
    , ", gasPrice = Nothing"
    , ", value = Nothing"
    , ", data = Just <|" <> encodeDataFunc <> sig <> " " <> encodings
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
