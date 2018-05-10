{-# LANGUAGE OverloadedStrings #-}

module Generator.Templates
    ( moduleNameAndExports
    , imports
    , callBuilder
    , logFilterBuilder
    ) where

import           Data.Monoid    ((<>))
import           Data.Text.Lazy (Text)
import qualified Utils          as U


-- | Declare module/contract name
moduleNameAndExports :: Text -> [Text] -> [Text]
moduleNameAndExports name (x:xs) =
    let
        exportHelper n = U.indent 2 (", " <> n)
    in
        [ "module " <> name
        , U.indent 1 "exposing"
        , U.indent 2  ("( " <> x)
        ] <> (exportHelper <$> xs) <> [ U.indent 2 ")\n" ]


-- | Elm imports
imports :: [Text]
imports =
    [ "import BigInt exposing (BigInt)"
    , "import Json.Decode as Decode exposing (Decoder)"
    , "import Json.Decode.Pipeline exposing (custom, decode)"
    , "import Eth.Types exposing (..)"
    , "import Eth.Utils as U"
    , "import Evm.Decode as Evm exposing (evmDecode, andMap, toElmDecoder, topic, data)"
    , "import Evm.Encode as Evm exposing (Encoding(..), evmEncode)"
    , ""
    , ""
    ]


-- | Return type for contract functions
callBuilder :: Bool -> Text -> Text -> Text -> [Text]
callBuilder isDebug sig encodings decoder =
    let
        encodeDataFunc =
            if isDebug then
                " Evm.encodeFunctionCallWithDebug "
            else
                " Evm.encodeFunctionCall "
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
