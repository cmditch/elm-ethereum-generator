module ERC20 exposing (..)

import BigInt exposing (BigInt)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode)
import Web3.Types exposing (..)
import Web3.Eth.Types exposing (..)
import Web3.Evm.Decode exposing (..)
import Web3.Evm.Encode as Evm exposing (..)
import Web3.Utils exposing (keccak256)


abi_ : String
abi_ =
    """[{"constant":true,"inputs":[],"name":"name","outputs":[{"name":"","type":"string"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_spender","type":"address"},{"name":"_value","type":"uint256"}],"name":"approve","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"totalSupply","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_from","type":"address"},{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transferFrom","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"}],"name":"balances","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"decimals","outputs":[{"name":"","type":"uint8"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"},{"name":"","type":"address"}],"name":"allowed","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"}],"name":"balanceOf","outputs":[{"name":"balance","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"symbol","outputs":[{"name":"","type":"string"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_to","type":"address"},{"name":"_value","type":"uint256"}],"name":"transfer","outputs":[{"name":"success","type":"bool"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"_owner","type":"address"},{"name":"_spender","type":"address"}],"name":"allowance","outputs":[{"name":"remaining","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[{"name":"_initialAmount","type":"uint256"},{"name":"_tokenName","type":"string"},{"name":"_decimalUnits","type":"uint8"},{"name":"_tokenSymbol","type":"string"}],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":true,"name":"_from","type":"address"},{"indexed":true,"name":"_to","type":"address"},{"indexed":false,"name":"_value","type":"uint256"}],"name":"Transfer","type":"event"},{"anonymous":false,"inputs":[{"indexed":true,"name":"_owner","type":"address"},{"indexed":true,"name":"_spender","type":"address"},{"indexed":false,"name":"_value","type":"uint256"}],"name":"Approval","type":"event"}]"""




allowance : Address -> Address -> Address -> Call BigInt
allowance contractAddress owner spender =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "allowance(address,address)" [ AddressE owner, AddressE spender ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


allowed : Address -> Address -> Address -> Call BigInt
allowed contractAddress a b =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "allowed(address,address)" [ AddressE a, AddressE b ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


approve : Address -> Address -> BigInt -> Call Bool
approve contractAddress spender value =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "approve(address,uint256)" [ AddressE spender, UintE value ]
    , nonce = Nothing
    , decoder = toElmDecoder bool
    }


balanceOf : Address -> Address -> Call BigInt
balanceOf contractAddress owner =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "balanceOf(address)" [ AddressE owner ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


balances : Address -> Address -> Call BigInt
balances contractAddress a =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "balances(address)" [ AddressE a ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


decimals : Address -> Call BigInt
decimals contractAddress  =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "decimals()" []
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


name : Address -> Call string-ERROR!
name contractAddress  =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "name()" []
    , nonce = Nothing
    , decoder = toElmDecoder string-ERROR!
    }


symbol : Address -> Call string-ERROR!
symbol contractAddress  =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "symbol()" []
    , nonce = Nothing
    , decoder = toElmDecoder string-ERROR!
    }


totalSupply : Address -> Call BigInt
totalSupply contractAddress  =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "totalSupply()" []
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


transfer : Address -> Address -> BigInt -> Call Bool
transfer contractAddress to value =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "transfer(address,uint256)" [ AddressE to, UintE value ]
    , nonce = Nothing
    , decoder = toElmDecoder bool
    }


transferFrom : Address -> Address -> Address -> BigInt -> Call Bool
transferFrom contractAddress from to value =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "transferFrom(address,address,uint256)" [ AddressE from, AddressE to, UintE value ]
    , nonce = Nothing
    , decoder = toElmDecoder bool
    }


{- Approval event -}


subscribeApproval : ( Address, EventId ) -> Cmd msg
subscribeApproval =
    Contract.subscribe abi_ "Approval"


onceApproval : Contract.Params (EventLog { owner : Address, spender : Address, value : BigInt })
onceApproval =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData Approval []
    , nonce = Nothing
    , decoder = approvalDecoder
    }


decodeApproval : String -> Result Error (EventLog { owner : Address, spender : Address, value : BigInt })
decodeApproval response =
    response
        |> D.decodeString approvalDecoder
        |> Result.mapError (\e -> Error e)


approvalDecoder : D.Decoder (EventLog { owner : Address, spender : Address, value : BigInt })
approvalDecoder =
    decode (\owner spender value -> { owner = owner, spender = spender, value = value })
        |> required "_owner" address
        |> required "_spender" address
        |> required "_value" uint
        |> D.eventLogDecoder




{- Transfer event -}


subscribeTransfer : ( Address, EventId ) -> Cmd msg
subscribeTransfer =
    Contract.subscribe abi_ "Transfer"


onceTransfer : Contract.Params (EventLog { from : Address, to : Address, value : BigInt })
onceTransfer =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData Transfer []
    , nonce = Nothing
    , decoder = transferDecoder
    }


decodeTransfer : String -> Result Error (EventLog { from : Address, to : Address, value : BigInt })
decodeTransfer response =
    response
        |> D.decodeString transferDecoder
        |> Result.mapError (\e -> Error e)


transferDecoder : D.Decoder (EventLog { from : Address, to : Address, value : BigInt })
transferDecoder =
    decode (\from to value -> { from = from, to = to, value = value })
        |> required "_from" address
        |> required "_to" address
        |> required "_value" uint
        |> D.eventLogDecoder



