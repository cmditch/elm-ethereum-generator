module EtherDelta
    exposing
        ( accountLevelsAddr
        , admin
        , amountFilled
        , availableVolume
        , balanceOf
        , cancelOrder
        , changeAccountLevelsAddr
        , changeAdmin
        , changeFeeAccount
        , changeFeeMake
        , changeFeeRebate
        , changeFeeTake
        , deposit
        , depositToken
        , feeAccount
        , feeMake
        , feeRebate
        , feeTake
        , order
        , OrderFills
        , orderFills
        , orderFillsDecoder
        , orders
        , testTrade
        , tokens
        , trade
        , withdraw
        , withdrawToken
        , Cancel
        , cancelEvent
        , cancelDecoder
        , Deposit
        , depositEvent
        , depositDecoder
        , Order
        , orderEvent
        , orderDecoder
        , Trade
        , tradeEvent
        , tradeDecoder
        , Withdraw
        , withdrawEvent
        , withdrawDecoder
        )

import BigInt exposing (BigInt)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode)
import Web3.Eth.Types exposing (..)
import Web3.Evm.Decode exposing (..)
import Web3.Evm.Encode as Evm exposing (..)
import Web3.Utils exposing (keccak256)


{-| "accountLevelsAddr()" function
-}
accountLevelsAddr : Address -> Call Address
accountLevelsAddr contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "accountLevelsAddr()" []
    , nonce = Nothing
    , decoder = toElmDecoder address
    }


{-| "admin()" function
-}
admin : Address -> Call Address
admin contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "admin()" []
    , nonce = Nothing
    , decoder = toElmDecoder address
    }


{-| "amountFilled(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)" function
-}
amountFilled : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> String -> String -> Call BigInt
amountFilled contractAddress tokenGet amountGet tokenGive amountGive expires nonce user v r s =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "amountFilled(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce, AddressE user, UintE v, BytesE r, BytesE s ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


{-| "availableVolume(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)" function
-}
availableVolume : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> String -> String -> Call BigInt
availableVolume contractAddress tokenGet amountGet tokenGive amountGive expires nonce user v r s =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "availableVolume(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce, AddressE user, UintE v, BytesE r, BytesE s ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


{-| "balanceOf(address,address)" function
-}
balanceOf : Address -> Address -> Address -> Call BigInt
balanceOf contractAddress token user =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "balanceOf(address,address)" [ AddressE token, AddressE user ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


{-| "cancelOrder(address,uint256,address,uint256,uint256,uint256,uint8,bytes32,bytes32)" function
-}
cancelOrder : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> BigInt -> String -> String -> Call ()
cancelOrder contractAddress tokenGet amountGet tokenGive amountGive expires nonce v r s =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "cancelOrder(address,uint256,address,uint256,uint256,uint256,uint8,bytes32,bytes32)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce, UintE v, BytesE r, BytesE s ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "changeAccountLevelsAddr(address)" function
-}
changeAccountLevelsAddr : Address -> Address -> Call ()
changeAccountLevelsAddr contractAddress accountLevelsAddr_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeAccountLevelsAddr(address)" [ AddressE accountLevelsAddr_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "changeAdmin(address)" function
-}
changeAdmin : Address -> Address -> Call ()
changeAdmin contractAddress admin_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeAdmin(address)" [ AddressE admin_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "changeFeeAccount(address)" function
-}
changeFeeAccount : Address -> Address -> Call ()
changeFeeAccount contractAddress feeAccount_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeFeeAccount(address)" [ AddressE feeAccount_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "changeFeeMake(uint256)" function
-}
changeFeeMake : Address -> BigInt -> Call ()
changeFeeMake contractAddress feeMake_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeFeeMake(uint256)" [ UintE feeMake_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "changeFeeRebate(uint256)" function
-}
changeFeeRebate : Address -> BigInt -> Call ()
changeFeeRebate contractAddress feeRebate_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeFeeRebate(uint256)" [ UintE feeRebate_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "changeFeeTake(uint256)" function
-}
changeFeeTake : Address -> BigInt -> Call ()
changeFeeTake contractAddress feeTake_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeFeeTake(uint256)" [ UintE feeTake_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "deposit()" function
-}
deposit : Address -> Call ()
deposit contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "deposit()" []
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "depositToken(address,uint256)" function
-}
depositToken : Address -> Address -> BigInt -> Call ()
depositToken contractAddress token amount =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "depositToken(address,uint256)" [ AddressE token, UintE amount ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "feeAccount()" function
-}
feeAccount : Address -> Call Address
feeAccount contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "feeAccount()" []
    , nonce = Nothing
    , decoder = toElmDecoder address
    }


{-| "feeMake()" function
-}
feeMake : Address -> Call BigInt
feeMake contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "feeMake()" []
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


{-| "feeRebate()" function
-}
feeRebate : Address -> Call BigInt
feeRebate contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "feeRebate()" []
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


{-| "feeTake()" function
-}
feeTake : Address -> Call BigInt
feeTake contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "feeTake()" []
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


{-| "order(address,uint256,address,uint256,uint256,uint256)" function
-}
order : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Call ()
order contractAddress tokenGet amountGet tokenGive amountGive expires nonce =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "order(address,uint256,address,uint256,uint256,uint256)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "orderFills(address,bytes32)" function
-}
type alias OrderFills =
    { v0 : BigInt
    , v1 : BigInt
    , v2 : BigInt
    }


orderFills : Address -> Address -> String -> Call OrderFills
orderFills contractAddress a b =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "orderFills(address,bytes32)" [ AddressE a, BytesE b ]
    , nonce = Nothing
    , decoder = orderFillsDecoder
    }


orderFillsDecoder : Decoder OrderFills
orderFillsDecoder =
    evmDecode OrderFills
        |> andMap uint
        |> andMap uint
        |> andMap uint
        |> toElmDecoder


{-| "orders(address,bytes32)" function
-}
orders : Address -> Address -> String -> Call Bool
orders contractAddress a b =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "orders(address,bytes32)" [ AddressE a, BytesE b ]
    , nonce = Nothing
    , decoder = toElmDecoder bool
    }


{-| "testTrade(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32,uint256,address)" function
-}
testTrade : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> String -> String -> BigInt -> Address -> Call Bool
testTrade contractAddress tokenGet amountGet tokenGive amountGive expires nonce user v r s amount sender =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "testTrade(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32,uint256,address)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce, AddressE user, UintE v, BytesE r, BytesE s, UintE amount, AddressE sender ]
    , nonce = Nothing
    , decoder = toElmDecoder bool
    }


{-| "tokens(address,address)" function
-}
tokens : Address -> Address -> Address -> Call BigInt
tokens contractAddress a b =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "tokens(address,address)" [ AddressE a, AddressE b ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


{-| "trade(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32,uint256)" function
-}
trade : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> String -> String -> BigInt -> Call ()
trade contractAddress tokenGet amountGet tokenGive amountGive expires nonce user v r s amount =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "trade(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32,uint256)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce, AddressE user, UintE v, BytesE r, BytesE s, UintE amount ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "withdraw(uint256)" function
-}
withdraw : Address -> BigInt -> Call ()
withdraw contractAddress amount =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "withdraw(uint256)" [ UintE amount ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "withdrawToken(address,uint256)" function
-}
withdrawToken : Address -> Address -> BigInt -> Call ()
withdrawToken contractAddress token amount =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "withdrawToken(address,uint256)" [ AddressE token, UintE amount ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "Cancel(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)" event
-}
type alias Cancel =
    { tokenGet : Address
    , amountGet : BigInt
    , tokenGive : Address
    , amountGive : BigInt
    , expires : BigInt
    , nonce : BigInt
    , user : Address
    , v : BigInt
    , r : String
    , s : String
    }


cancelEvent : Address -> LogFilter
cancelEvent contractAddress =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = [ Just <| keccak256 "Cancel(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)" ]
    }


cancelDecoder : Decoder Cancel
cancelDecoder =
    decode Cancel
        |> custom (data 0 address)
        |> custom (data 1 uint)
        |> custom (data 2 address)
        |> custom (data 3 uint)
        |> custom (data 4 uint)
        |> custom (data 5 uint)
        |> custom (data 6 address)
        |> custom (data 7 uint)
        |> custom (data 8 bytes)
        |> custom (data 9 bytes)


{-| "Deposit(address,address,uint256,uint256)" event
-}
type alias Deposit =
    { token : Address
    , user : Address
    , amount : BigInt
    , balance : BigInt
    }


depositEvent : Address -> LogFilter
depositEvent contractAddress =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = [ Just <| keccak256 "Deposit(address,address,uint256,uint256)" ]
    }


depositDecoder : Decoder Deposit
depositDecoder =
    decode Deposit
        |> custom (data 0 address)
        |> custom (data 1 address)
        |> custom (data 2 uint)
        |> custom (data 3 uint)


{-| "Order(address,uint256,address,uint256,uint256,uint256,address)" event
-}
type alias Order =
    { tokenGet : Address
    , amountGet : BigInt
    , tokenGive : Address
    , amountGive : BigInt
    , expires : BigInt
    , nonce : BigInt
    , user : Address
    }


orderEvent : Address -> Maybe Address -> Maybe Address -> Maybe Address -> LogFilter
orderEvent contractAddress tokenGet tokenGive user =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics =
        [ Just <| keccak256 "Order(address,uint256,address,uint256,uint256,uint256,address)"
        , Maybe.map (Evm.encode << AddressE) tokenGet
        , Maybe.map (Evm.encode << AddressE) tokenGive
        , Maybe.map (Evm.encode << AddressE) user
        ]
    }


orderDecoder : Decoder Order
orderDecoder =
    decode Order
        |> custom (topic 1 address)
        |> custom (data 0 uint)
        |> custom (topic 2 address)
        |> custom (data 1 uint)
        |> custom (data 2 uint)
        |> custom (data 3 uint)
        |> custom (topic 3 address)


{-| "Trade(address,uint256,address,uint256,address,address)" event
-}
type alias Trade =
    { tokenGet : Address
    , amountGet : BigInt
    , tokenGive : Address
    , amountGive : BigInt
    , get : Address
    , give : Address
    }


tradeEvent : Address -> LogFilter
tradeEvent contractAddress =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = [ Just <| keccak256 "Trade(address,uint256,address,uint256,address,address)" ]
    }


tradeDecoder : Decoder Trade
tradeDecoder =
    decode Trade
        |> custom (data 0 address)
        |> custom (data 1 uint)
        |> custom (data 2 address)
        |> custom (data 3 uint)
        |> custom (data 4 address)
        |> custom (data 5 address)


{-| "Withdraw(address,address,uint256,uint256)" event
-}
type alias Withdraw =
    { token : Address
    , user : Address
    , amount : BigInt
    , balance : BigInt
    }


withdrawEvent : Address -> LogFilter
withdrawEvent contractAddress =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = [ Just <| keccak256 "Withdraw(address,address,uint256,uint256)" ]
    }


withdrawDecoder : Decoder Withdraw
withdrawDecoder =
    decode Withdraw
        |> custom (data 0 address)
        |> custom (data 1 address)
        |> custom (data 2 uint)
        |> custom (data 3 uint)
