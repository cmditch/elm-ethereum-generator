module EtherDelta exposing (..)

import BigInt exposing (BigInt)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode)
import Web3.Types exposing (..)
import Web3.Eth.Types exposing (..)
import Web3.Evm.Decode exposing (..)
import Web3.Evm.Encode as Evm exposing (..)
import Web3.Utils exposing (keccak256)


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


amountFilled : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> bytes32-ERROR! -> bytes32-ERROR! -> Call BigInt
amountFilled contractAddress tokenGet amountGet tokenGive amountGive expires nonce user v r s =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "amountFilled(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce, AddressE user, UintE v, bytes32-ERROR! r, bytes32-ERROR! s ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


availableVolume : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> bytes32-ERROR! -> bytes32-ERROR! -> Call BigInt
availableVolume contractAddress tokenGet amountGet tokenGive amountGive expires nonce user v r s =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "availableVolume(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce, AddressE user, UintE v, bytes32-ERROR! r, bytes32-ERROR! s ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


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


cancelOrder : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> BigInt -> bytes32-ERROR! -> bytes32-ERROR! -> Call ()
cancelOrder contractAddress tokenGet amountGet tokenGive amountGive expires nonce v r s =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "cancelOrder(address,uint256,address,uint256,uint256,uint256,uint8,bytes32,bytes32)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce, UintE v, bytes32-ERROR! r, bytes32-ERROR! s ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


changeAccountLevelsAddr : Address -> Address -> Call ()
changeAccountLevelsAddr contractAddress accountLevelsAddr_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeAccountLevelsAddr(address)" [ AddressE accountLevelsAddr_ ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


changeAdmin : Address -> Address -> Call ()
changeAdmin contractAddress admin_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeAdmin(address)" [ AddressE admin_ ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


changeFeeAccount : Address -> Address -> Call ()
changeFeeAccount contractAddress feeAccount_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeFeeAccount(address)" [ AddressE feeAccount_ ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


changeFeeMake : Address -> BigInt -> Call ()
changeFeeMake contractAddress feeMake_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeFeeMake(uint256)" [ UintE feeMake_ ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


changeFeeRebate : Address -> BigInt -> Call ()
changeFeeRebate contractAddress feeRebate_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeFeeRebate(uint256)" [ UintE feeRebate_ ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


changeFeeTake : Address -> BigInt -> Call ()
changeFeeTake contractAddress feeTake_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "changeFeeTake(uint256)" [ UintE feeTake_ ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


deposit : Address -> Call ()
deposit contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "deposit()" []
    , nonce = Nothing
    , decoder = D.succeed ()
    }


depositToken : Address -> Address -> BigInt -> Call ()
depositToken contractAddress token amount =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "depositToken(address,uint256)" [ AddressE token, UintE amount ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


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


order : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Call ()
order contractAddress tokenGet amountGet tokenGive amountGive expires nonce =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "order(address,uint256,address,uint256,uint256,uint256)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


orderFills : Address -> Address -> bytes32-ERROR! -> Call BigInt
orderFills contractAddress a b =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "orderFills(address,bytes32)" [ AddressE a, bytes32-ERROR! b ]
    , nonce = Nothing
    , decoder = toElmDecoder uint
    }


orders : Address -> Address -> bytes32-ERROR! -> Call Bool
orders contractAddress a b =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "orders(address,bytes32)" [ AddressE a, bytes32-ERROR! b ]
    , nonce = Nothing
    , decoder = toElmDecoder bool
    }


testTrade : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> bytes32-ERROR! -> bytes32-ERROR! -> BigInt -> Address -> Call Bool
testTrade contractAddress tokenGet amountGet tokenGive amountGive expires nonce user v r s amount sender =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "testTrade(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32,uint256,address)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce, AddressE user, UintE v, bytes32-ERROR! r, bytes32-ERROR! s, UintE amount, AddressE sender ]
    , nonce = Nothing
    , decoder = toElmDecoder bool
    }


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


trade : Address -> Address -> BigInt -> Address -> BigInt -> BigInt -> BigInt -> Address -> BigInt -> bytes32-ERROR! -> bytes32-ERROR! -> BigInt -> Call ()
trade contractAddress tokenGet amountGet tokenGive amountGive expires nonce user v r s amount =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "trade(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32,uint256)" [ AddressE tokenGet, UintE amountGet, AddressE tokenGive, UintE amountGive, UintE expires, UintE nonce, AddressE user, UintE v, bytes32-ERROR! r, bytes32-ERROR! s, UintE amount ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


withdraw : Address -> BigInt -> Call ()
withdraw contractAddress amount =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "withdraw(uint256)" [ UintE amount ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


withdrawToken : Address -> Address -> BigInt -> Call ()
withdrawToken contractAddress token amount =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| encodeData "withdrawToken(address,uint256)" [ AddressE token, UintE amount ]
    , nonce = Nothing
    , decoder = D.succeed ()
    }


{- Cancel event -}


cancel :  LogFilter
cancel =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = [ Just <| keccak256 "Cancel(address,uint256,address,uint256,uint256,uint256,address,uint8,bytes32,bytes32)" ]
    }





{- Deposit event -}


deposit :  LogFilter
deposit =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = [ Just <| keccak256 "Deposit(address,address,uint256,uint256)" ]
    }





{- Order event -}


order : Maybe Address ->  Maybe BigInt ->  Maybe Address ->  LogFilter
ordertokenGet amountGet tokenGive =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics =
    [ Just <| keccak256 "Order(address,uint256,address,uint256,uint256,uint256,address)"
    , Maybe.map (Evm.encode << AddressE) tokenGet
    , Maybe.map (Evm.encode << UintE) amountGet
    , Maybe.map (Evm.encode << AddressE) tokenGive
    ]

    }





{- Trade event -}


trade :  LogFilter
trade =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = [ Just <| keccak256 "Trade(address,uint256,address,uint256,address,address)" ]
    }





{- Withdraw event -}


withdraw :  LogFilter
withdraw =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = [ Just <| keccak256 "Withdraw(address,address,uint256,uint256)" ]
    }






