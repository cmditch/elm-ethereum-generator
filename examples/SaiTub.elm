module Contracts.SaiTub
    exposing
        ( air
        , ask
        , authority
        , axe
        , bid
        , bite
        , cage
        , cap
        , chi
        , cupi
        , Cup
        , cups
        , cupsDecoder
        , din
        , draw
        , drip
        , era
        , exit
        , fee
        , fit
        , flow
        , free
        , gap
        , gem
        , give
        , gov
        , ink
        , join
        , lad
        , lock
        , mat
        , mold
        , off
        , open
        , out
        , owner
        , pep
        , per
        , pie
        , pip
        , pit
        , rap
        , rhi
        , rho
        , rum
        , safe
        , sai
        , setAuthority
        , setOwner
        , setPep
        , setPip
        , setVox
        , shut
        , sin
        , skr
        , tab
        , tag
        , tap
        , tax
        , turn
        , vox
        , wipe
        , LogNewCup
        , logNewCupEvent
        , logNewCupDecoder
        , LogNote
        , logNoteEvent
        , logNoteDecoder
        , LogSetAuthority
        , logSetAuthorityEvent
        , logSetAuthorityDecoder
        , LogSetOwner
        , logSetOwnerEvent
        , logSetOwnerDecoder
        )

import BigInt exposing (BigInt)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, decode)
import Eth.Types exposing (..)
import Eth.Utils as U
import Abi.Decode as AbiDecode exposing (abiDecode, andMap, toElmDecoder, topic, data)
import Abi.Encode as AbiEncode exposing (Encoding(..), abiEncode)


{-

   This file was generated by https://github.com/cmditch/elm-ethereum-generator

-}


{-| "air()" function
-}
air : Address -> Call BigInt
air contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "air()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "ask(uint256)" function
-}
ask : Address -> BigInt -> Call BigInt
ask contractAddress wad =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "ask(uint256)" [ AbiEncode.uint wad ]
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "authority()" function
-}
authority : Address -> Call Address
authority contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "authority()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "axe()" function
-}
axe : Address -> Call BigInt
axe contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "axe()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "bid(uint256)" function
-}
bid : Address -> BigInt -> Call BigInt
bid contractAddress wad =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "bid(uint256)" [ AbiEncode.uint wad ]
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "bite(bytes32)" function
-}
bite : Address -> String -> Call ()
bite contractAddress cup =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "bite(bytes32)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "cage(uint256,uint256)" function
-}
cage : Address -> BigInt -> BigInt -> Call ()
cage contractAddress fit_ jam =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "cage(uint256,uint256)" [ AbiEncode.uint fit_, AbiEncode.uint jam ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "cap()" function
-}
cap : Address -> Call BigInt
cap contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "cap()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "chi()" function
-}
chi : Address -> Call BigInt
chi contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "chi()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "cupi()" function
-}
cupi : Address -> Call BigInt
cupi contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "cupi()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "cups(bytes32)" function
-}
type alias Cup =
    { lad : Address
    , ink : BigInt
    , art : BigInt
    , ire : BigInt
    }


cups : Address -> String -> Call Cup
cups contractAddress a =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "cups(bytes32)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry a ]
    , nonce = Nothing
    , decoder = cupsDecoder
    }


cupsDecoder : Decoder Cup
cupsDecoder =
    abiDecode Cup
        |> andMap AbiDecode.address
        |> andMap AbiDecode.uint
        |> andMap AbiDecode.uint
        |> andMap AbiDecode.uint
        |> toElmDecoder


{-| "din()" function
-}
din : Address -> Call BigInt
din contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "din()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "draw(bytes32,uint256)" function
-}
draw : Address -> String -> BigInt -> Call ()
draw contractAddress cup wad =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "draw(bytes32,uint256)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup, AbiEncode.uint wad ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "drip()" function
-}
drip : Address -> Call ()
drip contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "drip()" []
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "era()" function
-}
era : Address -> Call BigInt
era contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "era()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "exit(uint256)" function
-}
exit : Address -> BigInt -> Call ()
exit contractAddress wad =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "exit(uint256)" [ AbiEncode.uint wad ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "fee()" function
-}
fee : Address -> Call BigInt
fee contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "fee()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "fit()" function
-}
fit : Address -> Call BigInt
fit contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "fit()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "flow()" function
-}
flow : Address -> Call ()
flow contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "flow()" []
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "free(bytes32,uint256)" function
-}
free : Address -> String -> BigInt -> Call ()
free contractAddress cup wad =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "free(bytes32,uint256)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup, AbiEncode.uint wad ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "gap()" function
-}
gap : Address -> Call BigInt
gap contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "gap()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "gem()" function
-}
gem : Address -> Call Address
gem contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "gem()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "give(bytes32,address)" function
-}
give : Address -> String -> Address -> Call ()
give contractAddress cup guy =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "give(bytes32,address)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup, AbiEncode.address guy ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "gov()" function
-}
gov : Address -> Call Address
gov contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "gov()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "ink(bytes32)" function
-}
ink : Address -> String -> Call BigInt
ink contractAddress cup =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "ink(bytes32)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup ]
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "join(uint256)" function
-}
join : Address -> BigInt -> Call ()
join contractAddress wad =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "join(uint256)" [ AbiEncode.uint wad ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "lad(bytes32)" function
-}
lad : Address -> String -> Call Address
lad contractAddress cup =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "lad(bytes32)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup ]
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "lock(bytes32,uint256)" function
-}
lock : Address -> String -> BigInt -> Call ()
lock contractAddress cup wad =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "lock(bytes32,uint256)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup, AbiEncode.uint wad ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "mat()" function
-}
mat : Address -> Call BigInt
mat contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "mat()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "mold(bytes32,uint256)" function
-}
mold : Address -> String -> BigInt -> Call ()
mold contractAddress param val =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "mold(bytes32,uint256)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry param, AbiEncode.uint val ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "off()" function
-}
off : Address -> Call Bool
off contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "off()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.bool
    }


{-| "open()" function
-}
open : Address -> Call String
open contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "open()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.staticBytes manually-enter-length-for-now-sry
    }


{-| "out()" function
-}
out : Address -> Call Bool
out contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "out()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.bool
    }


{-| "owner()" function
-}
owner : Address -> Call Address
owner contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "owner()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "pep()" function
-}
pep : Address -> Call Address
pep contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "pep()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "per()" function
-}
per : Address -> Call BigInt
per contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "per()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "pie()" function
-}
pie : Address -> Call BigInt
pie contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "pie()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "pip()" function
-}
pip : Address -> Call Address
pip contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "pip()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "pit()" function
-}
pit : Address -> Call Address
pit contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "pit()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "rap(bytes32)" function
-}
rap : Address -> String -> Call BigInt
rap contractAddress cup =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "rap(bytes32)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup ]
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "rhi()" function
-}
rhi : Address -> Call BigInt
rhi contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "rhi()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "rho()" function
-}
rho : Address -> Call BigInt
rho contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "rho()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "rum()" function
-}
rum : Address -> Call BigInt
rum contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "rum()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "safe(bytes32)" function
-}
safe : Address -> String -> Call Bool
safe contractAddress cup =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "safe(bytes32)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup ]
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.bool
    }


{-| "sai()" function
-}
sai : Address -> Call Address
sai contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "sai()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "setAuthority(address)" function
-}
setAuthority : Address -> Address -> Call ()
setAuthority contractAddress authority_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "setAuthority(address)" [ AbiEncode.address authority_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "setOwner(address)" function
-}
setOwner : Address -> Address -> Call ()
setOwner contractAddress owner_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "setOwner(address)" [ AbiEncode.address owner_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "setPep(address)" function
-}
setPep : Address -> Address -> Call ()
setPep contractAddress pep_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "setPep(address)" [ AbiEncode.address pep_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "setPip(address)" function
-}
setPip : Address -> Address -> Call ()
setPip contractAddress pip_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "setPip(address)" [ AbiEncode.address pip_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "setVox(address)" function
-}
setVox : Address -> Address -> Call ()
setVox contractAddress vox_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "setVox(address)" [ AbiEncode.address vox_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "shut(bytes32)" function
-}
shut : Address -> String -> Call ()
shut contractAddress cup =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "shut(bytes32)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "sin()" function
-}
sin : Address -> Call Address
sin contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "sin()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "skr()" function
-}
skr : Address -> Call Address
skr contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "skr()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "tab(bytes32)" function
-}
tab : Address -> String -> Call BigInt
tab contractAddress cup =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "tab(bytes32)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup ]
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "tag()" function
-}
tag : Address -> Call BigInt
tag contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "tag()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "tap()" function
-}
tap : Address -> Call Address
tap contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "tap()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "tax()" function
-}
tax : Address -> Call BigInt
tax contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "tax()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.uint
    }


{-| "turn(address)" function
-}
turn : Address -> Address -> Call ()
turn contractAddress tap_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "turn(address)" [ AbiEncode.address tap_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "vox()" function
-}
vox : Address -> Call Address
vox contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "vox()" []
    , nonce = Nothing
    , decoder = toElmDecoder AbiDecode.address
    }


{-| "wipe(bytes32,uint256)" function
-}
wipe : Address -> String -> BigInt -> Call ()
wipe contractAddress cup wad =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| AbiEncode.functionCall "wipe(bytes32,uint256)" [ AbiEncode.staticBytes manually-enter-length-for-now-sry cup, AbiEncode.uint wad ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }


{-| "LogNewCup(address,bytes32)" event
-}
type alias LogNewCup =
    { lad : Address
    , cup : String
    }


logNewCupEvent : Address -> Maybe Address -> LogFilter
logNewCupEvent contractAddress lad = 
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = 
        [ Just <| U.keccak256 "LogNewCup(address,bytes32)"
        , Maybe.map (abiEncode << AbiEncode.address) lad
        ]
    }


logNewCupDecoder : Decoder LogNewCup
logNewCupDecoder = 
    decode LogNewCup
        |> custom (topic 1 AbiDecode.address)
        |> custom (data 0 AbiDecode.staticBytes manually-enter-length-for-now-sry)


{-| "LogNote(bytes4,address,bytes32,bytes32,uint256,bytes)" event
-}
type alias LogNote =
    { sig : String
    , guy : Address
    , foo : String
    , bar : String
    , wad : BigInt
    , fax : String
    }


logNoteEvent : Address -> Maybe String -> Maybe Address -> Maybe String -> Maybe String -> LogFilter
logNoteEvent contractAddress sig guy foo bar = 
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = 
        [ Just <| U.keccak256 "LogNote(bytes4,address,bytes32,bytes32,uint256,bytes)"
        , Maybe.map (abiEncode << AbiEncode.staticBytes manually-enter-length-for-now-sry) sig
        , Maybe.map (abiEncode << AbiEncode.address) guy
        , Maybe.map (abiEncode << AbiEncode.staticBytes manually-enter-length-for-now-sry) foo
        , Maybe.map (abiEncode << AbiEncode.staticBytes manually-enter-length-for-now-sry) bar
        ]
    }


logNoteDecoder : Decoder LogNote
logNoteDecoder = 
    decode LogNote
        |> custom (topic 1 AbiDecode.staticBytes manually-enter-length-for-now-sry)
        |> custom (topic 2 AbiDecode.address)
        |> custom (topic 3 AbiDecode.staticBytes manually-enter-length-for-now-sry)
        |> custom (topic 4 AbiDecode.staticBytes manually-enter-length-for-now-sry)
        |> custom (data 0 AbiDecode.uint)
        |> custom (data 1 AbiDecode.dynamicBytes)


{-| "LogSetAuthority(address)" event
-}
type alias LogSetAuthority =
    { authority : Address }


logSetAuthorityEvent : Address -> Maybe Address -> LogFilter
logSetAuthorityEvent contractAddress authority = 
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = 
        [ Just <| U.keccak256 "LogSetAuthority(address)"
        , Maybe.map (abiEncode << AbiEncode.address) authority
        ]
    }


logSetAuthorityDecoder : Decoder LogSetAuthority
logSetAuthorityDecoder = 
    decode LogSetAuthority
        |> custom (topic 1 AbiDecode.address)


{-| "LogSetOwner(address)" event
-}
type alias LogSetOwner =
    { owner : Address }


logSetOwnerEvent : Address -> Maybe Address -> LogFilter
logSetOwnerEvent contractAddress owner = 
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics = 
        [ Just <| U.keccak256 "LogSetOwner(address)"
        , Maybe.map (abiEncode << AbiEncode.address) owner
        ]
    }


logSetOwnerDecoder : Decoder LogSetOwner
logSetOwnerDecoder = 
    decode LogSetOwner
        |> custom (topic 1 AbiDecode.address)


