module ERC20 exposing (..)

import BigInt as BI exposing (BigInt)
import Json.Decode as D
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as E
import Web3.Types exposing (..)
import Web3
import Web3.Eth.Contract as Contract
import Web3.Eth as Eth
import Web3.Decoders as D
import Task exposing (Task)


abi_ : Abi
abi_ =
    Abi
        """[{"constant":true,"inputs":[],"name":"name","outputs":[{"name":"","type":"string"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_spender","type":"address"},{"name":"_value","type":"uint256"}],"name":"approve","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"totalSupply","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_from","type":"address"},{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transferFrom","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"}],"name":"balances","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"decimals","outputs":[{"name":"","type":"uint8"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"},{"name":"","type":"address"}],"name":"allowed","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"balance","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"symbol","outputs":[{"name":"","type":"string"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transfer","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"},{"name":"_spender","type":"address"}],"name":"allowance","outputs":[{"name":"remaining","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[{"name":"_initialAmount","type":"uint256"},{"name":"_tokenName","type":"string"},{"name":"_decimalUnits","type":"uint8"},{"name":"_tokenSymbol","type":"string"}],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"_from","type":"address"},{"indexed":true,"name":"_to","type":"address"},{"indexed":false,"name":"_value","type":"uint256"}],"name":"Transfer","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"_owner","type":"address"},{"indexed":true,"name":"_spender","type":"address"},{"indexed":false,"name":"_value","type":"uint256"}],"name":"Approval","type":"event"}]"""


type alias Constructor =
    { initialAmount : BigInt
    , tokenName : String
    , decimalUnits : BigInt
    , tokenSymbol : String
    }


allowance : Address -> Address -> Contract.Params BigInt
allowance owner spender =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "allowance(address,address)"
    , data = Nothing
    , params = [ E.encodeAddress owner, E.encodeAddress spender ]
    , decoder = D.bigIntDecoder
    }


allowed : Address -> Address -> Contract.Params BigInt
allowed a b =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "allowed(address,address)"
    , data = Nothing
    , params = [ E.encodeAddress a, E.encodeAddress b ]
    , decoder = D.bigIntDecoder
    }


approve : Address -> BigInt -> Contract.Params Bool
approve spender value =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "approve(address,uint256)"
    , data = Nothing
    , params = [ E.encodeAddress spender, E.encodeBigInt value ]
    , decoder = D.bool
    }


balanceOf : Address -> Contract.Params BigInt
balanceOf owner =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "balanceOf(address)"
    , data = Nothing
    , params = [ E.encodeAddress owner ]
    , decoder = D.bigIntDecoder
    }


balances : Address -> Contract.Params BigInt
balances a =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "balances(address)"
    , data = Nothing
    , params = [ E.encodeAddress a ]
    , decoder = D.bigIntDecoder
    }


decimals : Contract.Params BigInt
decimals =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "decimals()"
    , data = Nothing
    , params = []
    , decoder = D.bigIntDecoder
    }


name : Contract.Params String
name =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "name()"
    , data = Nothing
    , params = []
    , decoder = D.string
    }


symbol : Contract.Params String
symbol =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "symbol()"
    , data = Nothing
    , params = []
    , decoder = D.string
    }


totalSupply : Contract.Params BigInt
totalSupply =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "totalSupply()"
    , data = Nothing
    , params = []
    , decoder = D.bigIntDecoder
    }


transfer : Address -> BigInt -> Contract.Params Bool
transfer to value =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "transfer(address,uint256)"
    , data = Nothing
    , params = [ E.encodeAddress to, E.encodeBigInt value ]
    , decoder = D.bool
    }


transferFrom : Address -> Address -> BigInt -> Contract.Params Bool
transferFrom from to value =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "transferFrom(address,address,uint256)"
    , data = Nothing
    , params = [ E.encodeAddress from, E.encodeAddress to, E.encodeBigInt value ]
    , decoder = D.bool
    }



{- Approval event -}


subscribeApproval : ( Address, EventId ) -> Cmd msg
subscribeApproval =
    Contract.subscribe abi_ "Approval"


onceApproval : Contract.Params (EventLog { owner : Address, spender : Address, value : BigInt })
onceApproval =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "Approval"
    , data = Nothing
    , params = []
    , decoder = approvalDecoder
    }


decodeApproval : String -> Result Error (EventLog { owner : Address, spender : Address, value : BigInt })
decodeApproval response =
    response
        |> D.decodeString approvalDecoder
        |> Result.mapError (\e -> Error e)


approvalDecoder =
    decode (\owner spender value -> { owner = owner, spender = spender, value = value })
        |> required "_owner" D.addressDecoder
        |> required "_spender" D.addressDecoder
        |> required "_value" D.bigIntDecoder



{- Transfer event -}


subscribeTransfer : ( Address, EventId ) -> Cmd msg
subscribeTransfer =
    Contract.subscribe abi_ "Transfer"


onceTransfer : Contract.Params (EventLog { from : Address, to : Address, value : BigInt })
onceTransfer =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "Transfer"
    , data = Nothing
    , params = []
    , decoder = transferDecoder
    }


decodeTransfer : String -> Result Error (EventLog { from : Address, to : Address, value : BigInt })
decodeTransfer response =
    response
        |> D.decodeString transferDecoder
        |> Result.mapError (\e -> Error e)


transferDecoder =
    decode (\from to value -> { from = from, to = to, value = value })
        |> required "_from" D.addressDecoder
        |> required "_to" D.addressDecoder
        |> required "_value" D.bigIntDecoder



{- Contract Helper Functions -}


encodeContractABI : Constructor -> Task Error Hex
encodeContractABI { initialAmount, tokenName, decimalUnits, tokenSymbol } =
    Contract.encodeContractABI
        { abi = abi_
        , gasPrice = Just (BI.fromInt 300000000)
        , gas = Just 300000
        , methodName = Nothing
        , data = Nothing
        , params = [ E.encodeBigInt, E.string, E.encodeBigInt, E.string ]
        , decoder = E.hexDecoder
        }


estimateContractGas : Constructor -> Task Error Int
estimateContractGas { initialAmount, tokenName, decimalUnits, tokenSymbol } =
    Contract.estimateContractGas
        { abi = abi_
        , gasPrice = Just (BI.fromInt 300000000)
        , gas = Just 300000
        , methodName = Nothing
        , data = Nothing
        , params = [ E.encodeBigInt, E.string, E.encodeBigInt, E.string ]
        , decoder = E.hexDecoder
        }


deploy : Address -> Maybe BigInt -> Constructor -> Task Error ContractInfo
deploy from value constructor =
    let
        buildAndDeployTx : Task Error TxId
        buildAndDeployTx =
            estimateContractGas constructor
                |> Task.andThen
                    (\gasCost ->
                        encodeContractABI constructor
                            |> Task.andThen
                                (\data ->
                                    Eth.sendTransaction from
                                        { to = Nothing
                                        , value = value
                                        , gas = gasCost
                                        , data = Just data
                                        , gasPrice = Just 10000000000
                                        , chainId = Nothing
                                        , nonce = Nothing
                                        }
                                )
                    )

        failIfNothing : Error -> Maybe a -> Task Error a
        failIfNothing error maybeVal =
            case maybeVal of
                Nothing ->
                    Task.fail error

                Just a ->
                    Task.succeed a

        waitForTxReceipt : TxId -> Task Error TxReceipt
        waitForTxReceipt txId =
            Eth.getTransactionReceipt txId
                |> Task.andThen (failIfNothing (Error "No Tx Receipt still. Mining error. Network Congestion?"))
                |> Web3.retry { attempts = 30, sleep = 3 }

        returnContractInfo : TxReceipt -> Task Error ContractInfo
        returnContractInfo txReceipt =
            case txReceipt.contractAddress of
                Nothing ->
                    Task.fail (Error "No contract address in Tx Receipt. This error should never happen...")

                Just contractAddress ->
                    Task.succeed { txId = txReceipt.transactionHash, address = contractAddress }
    in
        buildAndDeployTx
            |> Task.andThen waitForTxReceipt
            |> Task.andThen returnContractInfo
