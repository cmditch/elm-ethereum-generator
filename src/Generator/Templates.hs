{-# LANGUAGE OverloadedStrings #-}

module Generator.Templates
    ( moduleName
    , imports
    , abi
    , paramRecord
    , contractDeployFunc
    ) where

import           Data.Monoid    ((<>))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Utils          (indent, minify)


-- | Declare module/contract name
moduleName :: Text -> [Text]
moduleName name =
    ["module " <> name <> " exposing (..)\n"]


-- | Elm imports
imports :: [Text]
imports =
    [ "import Web3"
    , "import BigInt as BI exposing (BigInt)"
    , "import Json.Decode.Pipeline exposing (decode, required)"
    , "import Json.Decode as D"
    , "import Json.Encode as E"
    , "import Web3.Decoders as D"
    , "import Web3.Encoders as E"
    , "import Web3.Types exposing (..)"
    , "import Web3.Eth.Contract as Contract"
    , "import Web3.Eth as Eth"
    , "import Task exposing (Task)"
    , "\n"
    ]


-- | Declare Abi value
abi :: Text -> [Text]
abi rawABI =
    [ "abi_ : Abi"
    , "abi_ ="
    , "    Abi"
    , "        \"\"\"" <> minify rawABI <> "\"\"\""
    , "\n"
    ]

-- | Return type for contract functions
paramRecord :: Text -> Text -> Text -> [Text]
paramRecord method params decoder =
    [ "{ abi = abi_"
    , ", gasPrice = Just (BI.fromInt 300000000)" -- need to make these customizable
    , ", gas = Just 300000" -- need to make these customizable
    , ", methodName = " <> method
    , ", data = Nothing"
    , ", params = " <> params
    , ", decoder = " <> decoder
    , "}"
    ]


-- | Function to deploy contract if byteCode is supplied
contractDeployFunc :: [Text]
contractDeployFunc =
    ["deploy : Address -> Maybe BigInt -> Constructor -> Task Error ContractInfo"
    ,"deploy from value constructor ="
    ,"    let"
    ,"        buildAndDeployTx : Task Error TxId"
    ,"        buildAndDeployTx ="
    ,"            estimateContractGas constructor"
    ,"                |> Task.andThen"
    ,"                    (\\gasCost ->"
    ,"                        encodeContractABI constructor"
    ,"                            |> Task.andThen"
    ,"                                (\\data ->"
    ,"                                    Eth.sendTransaction from"
    ,"                                        { to = Nothing"
    ,"                                        , value = value"
    ,"                                        , gas = gasCost"
    ,"                                        , data = Just data"
    ,"                                        , gasPrice = Just 10000000000"
    ,"                                        , chainId = Nothing"
    ,"                                        , nonce = Nothing"
    ,"                                        }"
    ,"                                )"
    ,"                    )"
    ,""
    ,"        failIfNothing : Error -> Maybe a -> Task Error a"
    ,"        failIfNothing error maybeVal ="
    ,"            case maybeVal of"
    ,"                Nothing ->"
    ,"                    Task.fail error"
    ,""
    ,"                Just a ->"
    ,"                    Task.succeed a"
    ,""
    ,"        waitForTxReceipt : TxId -> Task Error TxReceipt"
    ,"        waitForTxReceipt txId ="
    ,"            Eth.getTransactionReceipt txId"
    ,"                |> Task.andThen (failIfNothing (Error \"No Tx Receipt still. Mining error. Network Congestion?\"))"
    ,"                |> Web3.retry { attempts = 30, sleep = 3 }"
    ,""
    ,"        returnContractInfo : TxReceipt -> Task Error ContractInfo"
    ,"        returnContractInfo txReceipt ="
    ,"            case txReceipt.contractAddress of"
    ,"                Nothing ->"
    ,"                    Task.fail (Error \"No contract address in Tx Receipt. This error should never happen...\")"
    ,""
    ,"                Just contractAddress ->"
    ,"                    Task.succeed { txId = txReceipt.transactionHash, address = contractAddress }"
    ,"    in"
    ,"        buildAndDeployTx"
    ,"            |> Task.andThen waitForTxReceipt"
    ,"            |> Task.andThen returnContractInfo"
    ]
