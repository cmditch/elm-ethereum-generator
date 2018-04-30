module ERC20 exposing (..)

import BigInt exposing (BigInt)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode)
import Web3.Types exposing (..)
import Web3.Eth.Types exposing (..)
import Web3.Evm.Decode exposing (..)
import Web3.Evm.Encode as Evm exposing (..)
import Web3.Utils exposing (keccak256)


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
decimals contractAddress =
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
name contractAddress =
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
symbol contractAddress =
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
totalSupply contractAddress =
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


approvalEvent : Address -> Maybe Address -> Maybe Address -> LogFilter
approvalEvent contractAddress owner spender = 
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = 
        [ Just <| keccak256 "Approval(address,address,uint256)"
        , Maybe.map (Evm.encode << AddressE) owner
        , Maybe.map (Evm.encode << AddressE) spender
        ]
    }



approvalDecoder : Decoder Approval
approvalDecoder = 
    decode Approval
        |> custom (topic 1 address)
        |> custom (topic 2 address)
        |> custom (data 0 uint)




type alias Approval =
    { owner : Address
    , spender : Address
    , value : BigInt
    }




{- Transfer event -}


transferEvent : Address -> Maybe Address -> Maybe Address -> LogFilter
transferEvent contractAddress from to = 
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = 
        [ Just <| keccak256 "Transfer(address,address,uint256)"
        , Maybe.map (Evm.encode << AddressE) from
        , Maybe.map (Evm.encode << AddressE) to
        ]
    }



transferDecoder : Decoder Transfer
transferDecoder = 
    decode Transfer
        |> custom (topic 1 address)
        |> custom (topic 2 address)
        |> custom (data 0 uint)




type alias Transfer =
    { from : Address
    , to : Address
    , value : BigInt
    }



