module ERC20
    exposing
        ( allowance
        , allowed
        , approve
        , balanceOf
        , balances
        , decimals
        , name
        , symbol
        , totalSupply
        , transfer
        , transferFrom
        , Approval
        , approvalEvent
        , approvalDecoder
        , Transfer
        , transferEvent
        , transferDecoder
        )

import BigInt exposing (BigInt)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode)
import Eth.Types exposing (..)
import Eth.Utils as U
import Evm.Decode as Evm exposing (evmDecode, andMap, toElmDecoder, topic, data)
import Evm.Encode as Evm exposing (Encoding(..), evmEncode)


{-| "allowance(address,address)" function
-}
allowance : Address -> Address -> Address -> Call BigInt
allowance contractAddress owner spender =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "allowance(address,address)" [ AddressE owner, AddressE spender ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
    }


{-| "allowed(address,address)" function
-}
allowed : Address -> Address -> Address -> Call BigInt
allowed contractAddress a b =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "allowed(address,address)" [ AddressE a, AddressE b ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
    }


{-| "approve(address,uint256)" function
-}
approve : Address -> Address -> BigInt -> Call Bool
approve contractAddress spender value =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "approve(address,uint256)" [ AddressE spender, UintE value ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.bool
    }


{-| "balanceOf(address)" function
-}
balanceOf : Address -> Address -> Call BigInt
balanceOf contractAddress owner =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "balanceOf(address)" [ AddressE owner ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
    }


{-| "balances(address)" function
-}
balances : Address -> Address -> Call BigInt
balances contractAddress a =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "balances(address)" [ AddressE a ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
    }


{-| "decimals()" function
-}
decimals : Address -> Call BigInt
decimals contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "decimals()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
    }


{-| "name()" function
-}
name : Address -> Call String
name contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "name()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.string
    }


{-| "symbol()" function
-}
symbol : Address -> Call String
symbol contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "symbol()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.string
    }


{-| "totalSupply()" function
-}
totalSupply : Address -> Call BigInt
totalSupply contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "totalSupply()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
    }


{-| "transfer(address,uint256)" function
-}
transfer : Address -> Address -> BigInt -> Call Bool
transfer contractAddress to value =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "transfer(address,uint256)" [ AddressE to, UintE value ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.bool
    }


{-| "transferFrom(address,address,uint256)" function
-}
transferFrom : Address -> Address -> Address -> BigInt -> Call Bool
transferFrom contractAddress from to value =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "transferFrom(address,address,uint256)" [ AddressE from, AddressE to, UintE value ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.bool
    }


{-| "Approval(address,address,uint256)" event
-}
type alias Approval =
    { owner : Address
    , spender : Address
    , value : BigInt
    }


approvalEvent : Address -> Maybe Address -> Maybe Address -> LogFilter
approvalEvent contractAddress owner spender = 
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = 
        [ Just <| U.keccak256 "Approval(address,address,uint256)"
        , Maybe.map (evmEncode << AddressE) owner
        , Maybe.map (evmEncode << AddressE) spender
        ]
    }


approvalDecoder : Decoder Approval
approvalDecoder = 
    decode Approval
        |> custom (topic 1 Evm.address)
        |> custom (topic 2 Evm.address)
        |> custom (data 0 Evm.uint)


{-| "Transfer(address,address,uint256)" event
-}
type alias Transfer =
    { from : Address
    , to : Address
    , value : BigInt
    }


transferEvent : Address -> Maybe Address -> Maybe Address -> LogFilter
transferEvent contractAddress from to = 
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = 
        [ Just <| U.keccak256 "Transfer(address,address,uint256)"
        , Maybe.map (evmEncode << AddressE) from
        , Maybe.map (evmEncode << AddressE) to
        ]
    }


transferDecoder : Decoder Transfer
transferDecoder = 
    decode Transfer
        |> custom (topic 1 Evm.address)
        |> custom (topic 2 Evm.address)
        |> custom (data 0 Evm.uint)


