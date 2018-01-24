module EtherDelta exposing (..)

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
        """[{"constant":false,"inputs":[{"name":"tokenGet","type":"address"},{"name":"amountGet","type":"uint256"},{"name":"tokenGive","type":"address"},{"name":"amountGive","type":"uint256"},{"name":"expires","type":"uint256"},{"name":"nonce","type":"uint256"},{"name":"user","type":"address"},{"name":"v","type":"uint8"},{"name":"r","type":"bytes32"},{"name":"s","type":"bytes32"},{"name":"amount","type":"uint256"}],"name":"trade","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[{"name":"tokenGet","type":"address"},{"name":"amountGet","type":"uint256"},{"name":"tokenGive","type":"address"},{"name":"amountGive","type":"uint256"},{"name":"expires","type":"uint256"},{"name":"nonce","type":"uint256"}],"name":"order","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"},{"name":"","type":"bytes32"}],"name":"orderFills","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"tokenGet","type":"address"},{"name":"amountGet","type":"uint256"},{"name":"tokenGive","type":"address"},{"name":"amountGive","type":"uint256"},{"name":"expires","type":"uint256"},{"name":"nonce","type":"uint256"},{"name":"v","type":"uint8"},{"name":"r","type":"bytes32"},{"name":"s","type":"bytes32"}],"name":"cancelOrder","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[{"name":"amount","type":"uint256"}],"name":"withdraw","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[{"name":"token","type":"address"},{"name":"amount","type":"uint256"}],"name":"depositToken","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"tokenGet","type":"address"},{"name":"amountGet","type":"uint256"},{"name":"tokenGive","type":"address"},{"name":"amountGive","type":"uint256"},{"name":"expires","type":"uint256"},{"name":"nonce","type":"uint256"},{"name":"user","type":"address"},{"name":"v","type":"uint8"},{"name":"r","type":"bytes32"},{"name":"s","type":"bytes32"}],"name":"amountFilled","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"},{"name":"","type":"address"}],"name":"tokens","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"feeMake_","type":"uint256"}],"name":"changeFeeMake","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"feeMake","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"feeRebate_","type":"uint256"}],"name":"changeFeeRebate","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"feeAccount","outputs":[{"name":"","type":"address"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"tokenGet","type":"address"},{"name":"amountGet","type":"uint256"},{"name":"tokenGive","type":"address"},{"name":"amountGive","type":"uint256"},{"name":"expires","type":"uint256"},{"name":"nonce","type":"uint256"},{"name":"user","type":"address"},{"name":"v","type":"uint8"},{"name":"r","type":"bytes32"},{"name":"s","type":"bytes32"},{"name":"amount","type":"uint256"},{"name":"sender","type":"address"}],"name":"testTrade","outputs":[{"name":"","type":"bool"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"feeAccount_","type":"address"}],"name":"changeFeeAccount","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"feeRebate","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"feeTake_","type":"uint256"}],"name":"changeFeeTake","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[{"name":"admin_","type":"address"}],"name":"changeAdmin","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":false,"inputs":[{"name":"token","type":"address"},{"name":"amount","type":"uint256"}],"name":"withdrawToken","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"","type":"address"},{"name":"","type":"bytes32"}],"name":"orders","outputs":[{"name":"","type":"bool"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"feeTake","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[],"name":"deposit","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"accountLevelsAddr_","type":"address"}],"name":"changeAccountLevelsAddr","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[],"name":"accountLevelsAddr","outputs":[{"name":"","type":"address"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"token","type":"address"},{"name":"user","type":"address"}],"name":"balanceOf","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[],"name":"admin","outputs":[{"name":"","type":"address"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":true,"inputs":[{"name":"tokenGet","type":"address"},{"name":"amountGet","type":"uint256"},{"name":"tokenGive","type":"address"},{"name":"amountGive","type":"uint256"},{"name":"expires","type":"uint256"},{"name":"nonce","type":"uint256"},{"name":"user","type":"address"},{"name":"v","type":"uint8"},{"name":"r","type":"bytes32"},{"name":"s","type":"bytes32"}],"name":"availableVolume","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"view","type":"function"},{"inputs":[{"name":"admin_","type":"address"},{"name":"feeAccount_","type":"address"},{"name":"accountLevelsAddr_","type":"address"},{"name":"feeMake_","type":"uint256"},{"name":"feeTake_","type":"uint256"},{"name":"feeRebate_","type":"uint256"}],"payable":false,"stateMutability":"nonpayable","type":"constructor"},{"payable":false,"stateMutability":"nonpayable","type":"fallback"},{"anonymous":false,"inputs":[{"indexed":false,"name":"tokenGet","type":"address"},{"indexed":false,"name":"amountGet","type":"uint256"},{"indexed":false,"name":"tokenGive","type":"address"},{"indexed":false,"name":"amountGive","type":"uint256"},{"indexed":false,"name":"expires","type":"uint256"},{"indexed":false,"name":"nonce","type":"uint256"},{"indexed":false,"name":"user","type":"address"}],"name":"Order","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"tokenGet","type":"address"},{"indexed":false,"name":"amountGet","type":"uint256"},{"indexed":false,"name":"tokenGive","type":"address"},{"indexed":false,"name":"amountGive","type":"uint256"},{"indexed":false,"name":"expires","type":"uint256"},{"indexed":false,"name":"nonce","type":"uint256"},{"indexed":false,"name":"user","type":"address"},{"indexed":false,"name":"v","type":"uint8"},{"indexed":false,"name":"r","type":"bytes32"},{"indexed":false,"name":"s","type":"bytes32"}],"name":"Cancel","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"tokenGet","type":"address"},{"indexed":false,"name":"amountGet","type":"uint256"},{"indexed":false,"name":"tokenGive","type":"address"},{"indexed":false,"name":"amountGive","type":"uint256"},{"indexed":false,"name":"get","type":"address"},{"indexed":false,"name":"give","type":"address"}],"name":"Trade","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"token","type":"address"},{"indexed":false,"name":"user","type":"address"},{"indexed":false,"name":"amount","type":"uint256"},{"indexed":false,"name":"balance","type":"uint256"}],"name":"Deposit","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"token","type":"address"},{"indexed":false,"name":"user","type":"address"},{"indexed":false,"name":"amount","type":"uint256"},{"indexed":false,"name":"balance","type":"uint256"}],"name":"Withdraw","type":"event"}]"""


type alias Constructor =
    { admin_ : Address
    , feeAccount_ : Address
    , accountLevelsAddr_ : Address
    , feeMake_ : BigInt
    , feeTake_ : BigInt
    , feeRebate_ : BigInt
    }


accountLevelsAddr : Contract.Params Address
accountLevelsAddr =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "accountLevelsAddr()"
    , data = Nothing
    , params = []
    , decoder = D.addressDecoder
    }


admin : Contract.Params Address
admin =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "admin()"
    , data = Nothing
    , params = []
    , decoder = D.addressDecoder
    }


amountFilled : Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> Bytes -> Bytes -> Contract.Params BigInt
amountFilled tokenGet amountGet tokenGive amountGive expires nonce user v r s =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "amountFilled(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)"
    , data = Nothing
    , params = [ E.encodeAddress tokenGet, E.encodeBigInt amountGet, E.encodeAddress tokenGive, E.encodeBigInt amountGive, E.encodeBigInt expires, E.encodeBigInt nonce, E.encodeAddress user, E.encodeBigInt v, E.encodeBytes r, E.encodeBytes s ]
    , decoder = D.bigIntDecoder
    }


availableVolume : Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> Bytes -> Bytes -> Contract.Params BigInt
availableVolume tokenGet amountGet tokenGive amountGive expires nonce user v r s =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "availableVolume(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)"
    , data = Nothing
    , params = [ E.encodeAddress tokenGet, E.encodeBigInt amountGet, E.encodeAddress tokenGive, E.encodeBigInt amountGive, E.encodeBigInt expires, E.encodeBigInt nonce, E.encodeAddress user, E.encodeBigInt v, E.encodeBytes r, E.encodeBytes s ]
    , decoder = D.bigIntDecoder
    }


balanceOf : Address -> Address -> Contract.Params BigInt
balanceOf token user =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "balanceOf(address,address)"
    , data = Nothing
    , params = [ E.encodeAddress token, E.encodeAddress user ]
    , decoder = D.bigIntDecoder
    }


cancelOrder : Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> BigInt -> Bytes -> Bytes -> Contract.Params ()
cancelOrder tokenGet amountGet tokenGive amountGive expires nonce v r s =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "cancelOrder(address,uint256,address,uint256,uint256,uint256,uint8,bytes32,bytes32)"
    , data = Nothing
    , params = [ E.encodeAddress tokenGet, E.encodeBigInt amountGet, E.encodeAddress tokenGive, E.encodeBigInt amountGive, E.encodeBigInt expires, E.encodeBigInt nonce, E.encodeBigInt v, E.encodeBytes r, E.encodeBytes s ]
    , decoder = D.succeed ()
    }


changeAccountLevelsAddr : Address -> Contract.Params ()
changeAccountLevelsAddr accountLevelsAddr_ =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "changeAccountLevelsAddr(address)"
    , data = Nothing
    , params = [ E.encodeAddress accountLevelsAddr_ ]
    , decoder = D.succeed ()
    }


changeAdmin : Address -> Contract.Params ()
changeAdmin admin_ =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "changeAdmin(address)"
    , data = Nothing
    , params = [ E.encodeAddress admin_ ]
    , decoder = D.succeed ()
    }


changeFeeAccount : Address -> Contract.Params ()
changeFeeAccount feeAccount_ =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "changeFeeAccount(address)"
    , data = Nothing
    , params = [ E.encodeAddress feeAccount_ ]
    , decoder = D.succeed ()
    }


changeFeeMake : BigInt -> Contract.Params ()
changeFeeMake feeMake_ =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "changeFeeMake(uint256)"
    , data = Nothing
    , params = [ E.encodeBigInt feeMake_ ]
    , decoder = D.succeed ()
    }


changeFeeRebate : BigInt -> Contract.Params ()
changeFeeRebate feeRebate_ =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "changeFeeRebate(uint256)"
    , data = Nothing
    , params = [ E.encodeBigInt feeRebate_ ]
    , decoder = D.succeed ()
    }


changeFeeTake : BigInt -> Contract.Params ()
changeFeeTake feeTake_ =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "changeFeeTake(uint256)"
    , data = Nothing
    , params = [ E.encodeBigInt feeTake_ ]
    , decoder = D.succeed ()
    }


deposit : Contract.Params ()
deposit =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "deposit()"
    , data = Nothing
    , params = []
    , decoder = D.succeed ()
    }


depositToken : Address -> BigInt -> Contract.Params ()
depositToken token amount =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "depositToken(address,uint256)"
    , data = Nothing
    , params = [ E.encodeAddress token, E.encodeBigInt amount ]
    , decoder = D.succeed ()
    }


feeAccount : Contract.Params Address
feeAccount =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "feeAccount()"
    , data = Nothing
    , params = []
    , decoder = D.addressDecoder
    }


feeMake : Contract.Params BigInt
feeMake =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "feeMake()"
    , data = Nothing
    , params = []
    , decoder = D.bigIntDecoder
    }


feeRebate : Contract.Params BigInt
feeRebate =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "feeRebate()"
    , data = Nothing
    , params = []
    , decoder = D.bigIntDecoder
    }


feeTake : Contract.Params BigInt
feeTake =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "feeTake()"
    , data = Nothing
    , params = []
    , decoder = D.bigIntDecoder
    }


order : Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Contract.Params ()
order tokenGet amountGet tokenGive amountGive expires nonce =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "order(address,uint256,address,uint256,uint256,uint256)"
    , data = Nothing
    , params = [ E.encodeAddress tokenGet, E.encodeBigInt amountGet, E.encodeAddress tokenGive, E.encodeBigInt amountGive, E.encodeBigInt expires, E.encodeBigInt nonce ]
    , decoder = D.succeed ()
    }


orderFills : Address -> Bytes -> Contract.Params BigInt
orderFills a b =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "orderFills(address,bytes32)"
    , data = Nothing
    , params = [ E.encodeAddress a, E.encodeBytes b ]
    , decoder = D.bigIntDecoder
    }


orders : Address -> Bytes -> Contract.Params Bool
orders a b =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "orders(address,bytes32)"
    , data = Nothing
    , params = [ E.encodeAddress a, E.encodeBytes b ]
    , decoder = D.bool
    }


testTrade : Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> Bytes -> Bytes -> BigInt -> Address -> Contract.Params Bool
testTrade tokenGet amountGet tokenGive amountGive expires nonce user v r s amount sender =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "testTrade(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32,uint256,address)"
    , data = Nothing
    , params = [ E.encodeAddress tokenGet, E.encodeBigInt amountGet, E.encodeAddress tokenGive, E.encodeBigInt amountGive, E.encodeBigInt expires, E.encodeBigInt nonce, E.encodeAddress user, E.encodeBigInt v, E.encodeBytes r, E.encodeBytes s, E.encodeBigInt amount, E.encodeAddress sender ]
    , decoder = D.bool
    }


tokens : Address -> Address -> Contract.Params BigInt
tokens a b =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "tokens(address,address)"
    , data = Nothing
    , params = [ E.encodeAddress a, E.encodeAddress b ]
    , decoder = D.bigIntDecoder
    }


trade : Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> Bytes -> Bytes -> BigInt -> Contract.Params ()
trade tokenGet amountGet tokenGive amountGive expires nonce user v r s amount =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "trade(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32,uint256)"
    , data = Nothing
    , params = [ E.encodeAddress tokenGet, E.encodeBigInt amountGet, E.encodeAddress tokenGive, E.encodeBigInt amountGive, E.encodeBigInt expires, E.encodeBigInt nonce, E.encodeAddress user, E.encodeBigInt v, E.encodeBytes r, E.encodeBytes s, E.encodeBigInt amount ]
    , decoder = D.succeed ()
    }


withdraw : BigInt -> Contract.Params ()
withdraw amount =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "withdraw(uint256)"
    , data = Nothing
    , params = [ E.encodeBigInt amount ]
    , decoder = D.succeed ()
    }


withdrawToken : Address -> BigInt -> Contract.Params ()
withdrawToken token amount =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "withdrawToken(address,uint256)"
    , data = Nothing
    , params = [ E.encodeAddress token, E.encodeBigInt amount ]
    , decoder = D.succeed ()
    }



{- Cancel event -}


subscribeCancel : ( Address, EventId ) -> Cmd msg
subscribeCancel =
    Contract.subscribe abi_ "Cancel"


onceCancel : Contract.Params (EventLog { tokenGet : Address, amountGet : BigInt, tokenGive : Address, amountGive : BigInt, expires : BigInt, nonce : BigInt, user : Address, v : BigInt, r : Bytes, s : Bytes })
onceCancel =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "Cancel"
    , data = Nothing
    , params = []
    , decoder = cancelDecoder
    }


decodeCancel : String -> Result Error (EventLog { tokenGet : Address, amountGet : BigInt, tokenGive : Address, amountGive : BigInt, expires : BigInt, nonce : BigInt, user : Address, v : BigInt, r : Bytes, s : Bytes })
decodeCancel response =
    response
        |> D.decodeString cancelDecoder
        |> Result.mapError (\e -> Error e)


cancelDecoder =
    decode (\tokenGet amountGet tokenGive amountGive expires nonce user v r s -> { tokenGet = tokenGet, amountGet = amountGet, tokenGive = tokenGive, amountGive = amountGive, expires = expires, nonce = nonce, user = user, v = v, r = r, s = s })
        |> required "tokenGet" D.addressDecoder
        |> required "amountGet" D.bigIntDecoder
        |> required "tokenGive" D.addressDecoder
        |> required "amountGive" D.bigIntDecoder
        |> required "expires" D.bigIntDecoder
        |> required "nonce" D.bigIntDecoder
        |> required "user" D.addressDecoder
        |> required "v" D.bigIntDecoder
        |> required "r" D.bytesDecoder
        |> required "s" D.bytesDecoder



{- Deposit event -}


subscribeDeposit : ( Address, EventId ) -> Cmd msg
subscribeDeposit =
    Contract.subscribe abi_ "Deposit"


onceDeposit : Contract.Params (EventLog { token : Address, user : Address, amount : BigInt, balance : BigInt })
onceDeposit =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "Deposit"
    , data = Nothing
    , params = []
    , decoder = depositDecoder
    }


decodeDeposit : String -> Result Error (EventLog { token : Address, user : Address, amount : BigInt, balance : BigInt })
decodeDeposit response =
    response
        |> D.decodeString depositDecoder
        |> Result.mapError (\e -> Error e)


depositDecoder =
    decode (\token user amount balance -> { token = token, user = user, amount = amount, balance = balance })
        |> required "token" D.addressDecoder
        |> required "user" D.addressDecoder
        |> required "amount" D.bigIntDecoder
        |> required "balance" D.bigIntDecoder



{- Order event -}


subscribeOrder : ( Address, EventId ) -> Cmd msg
subscribeOrder =
    Contract.subscribe abi_ "Order"


onceOrder : Contract.Params (EventLog { tokenGet : Address, amountGet : BigInt, tokenGive : Address, amountGive : BigInt, expires : BigInt, nonce : BigInt, user : Address })
onceOrder =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "Order"
    , data = Nothing
    , params = []
    , decoder = orderDecoder
    }


decodeOrder : String -> Result Error (EventLog { tokenGet : Address, amountGet : BigInt, tokenGive : Address, amountGive : BigInt, expires : BigInt, nonce : BigInt, user : Address })
decodeOrder response =
    response
        |> D.decodeString orderDecoder
        |> Result.mapError (\e -> Error e)


orderDecoder =
    decode (\tokenGet amountGet tokenGive amountGive expires nonce user -> { tokenGet = tokenGet, amountGet = amountGet, tokenGive = tokenGive, amountGive = amountGive, expires = expires, nonce = nonce, user = user })
        |> required "tokenGet" D.addressDecoder
        |> required "amountGet" D.bigIntDecoder
        |> required "tokenGive" D.addressDecoder
        |> required "amountGive" D.bigIntDecoder
        |> required "expires" D.bigIntDecoder
        |> required "nonce" D.bigIntDecoder
        |> required "user" D.addressDecoder



{- Trade event -}


subscribeTrade : ( Address, EventId ) -> Cmd msg
subscribeTrade =
    Contract.subscribe abi_ "Trade"


onceTrade : Contract.Params (EventLog { tokenGet : Address, amountGet : BigInt, tokenGive : Address, amountGive : BigInt, get : Address, give : Address })
onceTrade =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "Trade"
    , data = Nothing
    , params = []
    , decoder = tradeDecoder
    }


decodeTrade : String -> Result Error (EventLog { tokenGet : Address, amountGet : BigInt, tokenGive : Address, amountGive : BigInt, get : Address, give : Address })
decodeTrade response =
    response
        |> D.decodeString tradeDecoder
        |> Result.mapError (\e -> Error e)


tradeDecoder =
    decode (\tokenGet amountGet tokenGive amountGive get give -> { tokenGet = tokenGet, amountGet = amountGet, tokenGive = tokenGive, amountGive = amountGive, get = get, give = give })
        |> required "tokenGet" D.addressDecoder
        |> required "amountGet" D.bigIntDecoder
        |> required "tokenGive" D.addressDecoder
        |> required "amountGive" D.bigIntDecoder
        |> required "get" D.addressDecoder
        |> required "give" D.addressDecoder



{- Withdraw event -}


subscribeWithdraw : ( Address, EventId ) -> Cmd msg
subscribeWithdraw =
    Contract.subscribe abi_ "Withdraw"


onceWithdraw : Contract.Params (EventLog { token : Address, user : Address, amount : BigInt, balance : BigInt })
onceWithdraw =
    { abi = abi_
    , gasPrice = Just (BI.fromInt 300000000)
    , gas = Just 300000
    , methodName = Just "Withdraw"
    , data = Nothing
    , params = []
    , decoder = withdrawDecoder
    }


decodeWithdraw : String -> Result Error (EventLog { token : Address, user : Address, amount : BigInt, balance : BigInt })
decodeWithdraw response =
    response
        |> D.decodeString withdrawDecoder
        |> Result.mapError (\e -> Error e)


withdrawDecoder =
    decode (\token user amount balance -> { token = token, user = user, amount = amount, balance = balance })
        |> required "token" D.addressDecoder
        |> required "user" D.addressDecoder
        |> required "amount" D.bigIntDecoder
        |> required "balance" D.bigIntDecoder



{- Contract Helper Functions -}


encodeContractABI : Constructor -> Task Error Hex
encodeContractABI { admin_, feeAccount_, accountLevelsAddr_, feeMake_, feeTake_, feeRebate_ } =
    Contract.encodeContractABI
        { abi = abi_
        , gasPrice = Just (BI.fromInt 300000000)
        , gas = Just 300000
        , methodName = Nothing
        , data = Nothing
        , params = [ E.encodeAddress, E.encodeAddress, E.encodeAddress, E.encodeBigInt, E.encodeBigInt, E.encodeBigInt ]
        , decoder = E.hexDecoder
        }


estimateContractGas : Constructor -> Task Error Int
estimateContractGas { admin_, feeAccount_, accountLevelsAddr_, feeMake_, feeTake_, feeRebate_ } =
    Contract.estimateContractGas
        { abi = abi_
        , gasPrice = Just (BI.fromInt 300000000)
        , gas = Just 300000
        , methodName = Nothing
        , data = Nothing
        , params = [ E.encodeAddress, E.encodeAddress, E.encodeAddress, E.encodeBigInt, E.encodeBigInt, E.encodeBigInt ]
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
