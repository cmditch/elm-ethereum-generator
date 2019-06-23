module Contracts.SaiTub exposing
    ( Cup
    , LogNewCup
    , LogNote
    , LogSetAuthority
    , LogSetOwner
    , air
    , ask
    , authority
    , axe
    , bid
    , bite
    , cage
    , cap
    , chi
    , cupi
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
    , logNewCupDecoder
    , logNewCupEvent
    , logNoteDecoder
    , logNoteEvent
    , logSetAuthorityDecoder
    , logSetAuthorityEvent
    , logSetOwnerDecoder
    , logSetOwnerEvent
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
    )

import BigInt exposing (BigInt)
import Eth.Abi.Decode as D exposing (abiDecode, andMap, data, toElmDecoder, topic)
import Eth.Abi.Encode as E exposing (Encoding(..), abiEncode)
import Eth.Types exposing (..)
import Eth.Utils as U
import Json.Decode as Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (custom)



{-

   This file was generated by https://github.com/cmditch/elm-ethereum-generator v3.0.0
   Compatible with elm-ethereum v4.0.0

-}
-- air() function


air : Address -> Call BigInt
air contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "27e7e21e" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- ask(uint256) function


ask : Address -> BigInt -> Call BigInt
ask contractAddress wad_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "e47e7e66" [ E.uint wad_ ]
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- authority() function


authority : Address -> Call Address
authority contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "bf7e214f" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- axe() function


axe : Address -> Call BigInt
axe contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "509bf2bf" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- bid(uint256) function


bid : Address -> BigInt -> Call BigInt
bid contractAddress wad_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "454a2ab3" [ E.uint wad_ ]
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- bite(bytes32) function


bite : Address -> Hex -> Call ()
bite contractAddress cup_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "40cc8854" [ E.staticBytes 32 cup_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- cage(uint256,uint256) function


cage : Address -> BigInt -> BigInt -> Call ()
cage contractAddress fit__ jam_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "8ceedb47" [ E.uint fit__, E.uint jam_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- cap() function


cap : Address -> Call BigInt
cap contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "355274ea" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- chi() function


chi : Address -> Call BigInt
chi contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "c92aecc4" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- cupi() function


cupi : Address -> Call BigInt
cupi contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "49955431" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- cups(bytes32) function


type alias Cup =
    { lad : Address
    , ink : BigInt
    , art : BigInt
    , ire : BigInt
    }


cups : Address -> Hex -> Call Cup
cups contractAddress a_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "fdac0025" [ E.staticBytes 32 a_ ]
    , nonce = Nothing
    , decoder = cupsDecoder
    }


cupsDecoder : Decoder Cup
cupsDecoder =
    abiDecode Cup
        |> andMap D.address
        |> andMap D.uint
        |> andMap D.uint
        |> andMap D.uint
        |> toElmDecoder



-- din() function


din : Address -> Call BigInt
din contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "e0ae96e9" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- draw(bytes32,uint256) function


draw : Address -> Hex -> BigInt -> Call ()
draw contractAddress cup_ wad_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "440f19ba" [ E.staticBytes 32 cup_, E.uint wad_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- drip() function


drip : Address -> Call ()
drip contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "9f678cca" []
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- era() function


era : Address -> Call BigInt
era contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "143e55e0" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- exit(uint256) function


exit : Address -> BigInt -> Call ()
exit contractAddress wad_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "7f8661a1" [ E.uint wad_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- fee() function


fee : Address -> Call BigInt
fee contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "ddca3f43" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- fit() function


fit : Address -> Call BigInt
fit contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "c8e13bb4" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- flow() function


flow : Address -> Call ()
flow contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "343aad82" []
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- free(bytes32,uint256) function


free : Address -> Hex -> BigInt -> Call ()
free contractAddress cup_ wad_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "a5cd184e" [ E.staticBytes 32 cup_, E.uint wad_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- gap() function


gap : Address -> Call BigInt
gap contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "6c32c0a6" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- gem() function


gem : Address -> Call Address
gem contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "7bd2bea7" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- give(bytes32,address) function


give : Address -> Hex -> Address -> Call ()
give contractAddress cup_ guy_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "baa8529c" [ E.staticBytes 32 cup_, E.address guy_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- gov() function


gov : Address -> Call Address
gov contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "12d43a51" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- ink(bytes32) function


ink : Address -> Hex -> Call BigInt
ink contractAddress cup_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "1f3634ed" [ E.staticBytes 32 cup_ ]
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- join(uint256) function


join : Address -> BigInt -> Call ()
join contractAddress wad_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "049878f3" [ E.uint wad_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- lad(bytes32) function


lad : Address -> Hex -> Call Address
lad contractAddress cup_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "de5f5517" [ E.staticBytes 32 cup_ ]
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- lock(bytes32,uint256) function


lock : Address -> Hex -> BigInt -> Call ()
lock contractAddress cup_ wad_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "b3b77a51" [ E.staticBytes 32 cup_, E.uint wad_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- mat() function


mat : Address -> Call BigInt
mat contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "ab0783da" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- mold(bytes32,uint256) function


mold : Address -> Hex -> BigInt -> Call ()
mold contractAddress param_ val_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "92b0d721" [ E.staticBytes 32 param_, E.uint val_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- off() function


off : Address -> Call Bool
off contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "6626b26d" []
    , nonce = Nothing
    , decoder = toElmDecoder D.bool
    }



-- open() function


open : Address -> Call Hex
open contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "fcfff16f" []
    , nonce = Nothing
    , decoder = toElmDecoder (D.staticBytes 32)
    }



-- out() function


out : Address -> Call Bool
out contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "b2a1449b" []
    , nonce = Nothing
    , decoder = toElmDecoder D.bool
    }



-- owner() function


owner : Address -> Call Address
owner contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "8da5cb5b" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- pep() function


pep : Address -> Call Address
pep contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "ace237f5" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- per() function


per : Address -> Call BigInt
per contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "7ec9c3b8" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- pie() function


pie : Address -> Call BigInt
pie contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "8a95a746" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- pip() function


pip : Address -> Call Address
pip contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "d741e2f9" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- pit() function


pit : Address -> Call Address
pit contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "f03c7c6e" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- rap(bytes32) function


rap : Address -> Hex -> Call BigInt
rap contractAddress cup_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "6f78ee0d" [ E.staticBytes 32 cup_ ]
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- rhi() function


rhi : Address -> Call BigInt
rhi contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "338a0261" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- rho() function


rho : Address -> Call BigInt
rho contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "20aba08b" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- rum() function


rum : Address -> Call BigInt
rum contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "8cf0c191" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- safe(bytes32) function


safe : Address -> Hex -> Call Bool
safe contractAddress cup_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "e95823ad" [ E.staticBytes 32 cup_ ]
    , nonce = Nothing
    , decoder = toElmDecoder D.bool
    }



-- sai() function


sai : Address -> Call Address
sai contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "9166cba4" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- setAuthority(address) function


setAuthority : Address -> Address -> Call ()
setAuthority contractAddress authority__ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "7a9e5e4b" [ E.address authority__ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- setOwner(address) function


setOwner : Address -> Address -> Call ()
setOwner contractAddress owner__ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "13af4035" [ E.address owner__ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- setPep(address) function


setPep : Address -> Address -> Call ()
setPep contractAddress pep__ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "d9c27cc6" [ E.address pep__ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- setPip(address) function


setPip : Address -> Address -> Call ()
setPip contractAddress pip__ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "82bf9a75" [ E.address pip__ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- setVox(address) function


setVox : Address -> Address -> Call ()
setVox contractAddress vox__ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "cf48d1a6" [ E.address vox__ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- shut(bytes32) function


shut : Address -> Hex -> Call ()
shut contractAddress cup_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "b84d2106" [ E.staticBytes 32 cup_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- sin() function


sin : Address -> Call Address
sin contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "071bafb5" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- skr() function


skr : Address -> Call Address
skr contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "0f8a771e" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- tab(bytes32) function


tab : Address -> Hex -> Call BigInt
tab contractAddress cup_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "f7c8d634" [ E.staticBytes 32 cup_ ]
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- tag() function


tag : Address -> Call BigInt
tag contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "51f91066" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- tap() function


tap : Address -> Call Address
tap contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "fd221031" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- tax() function


tax : Address -> Call BigInt
tax contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "99c8d556" []
    , nonce = Nothing
    , decoder = toElmDecoder D.uint
    }



-- turn(address) function


turn : Address -> Address -> Call ()
turn contractAddress tap__ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "7e74325f" [ E.address tap__ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- vox() function


vox : Address -> Call Address
vox contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "67550a35" []
    , nonce = Nothing
    , decoder = toElmDecoder D.address
    }



-- wipe(bytes32,uint256) function


wipe : Address -> Hex -> BigInt -> Call ()
wipe contractAddress cup_ wad_ =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| E.functionCall "73b38101" [ E.staticBytes 32 cup_, E.uint wad_ ]
    , nonce = Nothing
    , decoder = Decode.succeed ()
    }



-- LogNewCup(address,bytes32) event


type alias LogNewCup =
    { lad : Address
    , cup : Hex
    }


logNewCupEvent : Address -> Maybe Address -> LogFilter
logNewCupEvent contractAddress lad_ =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics =
        [ Just <| U.unsafeToHex "89b8893b806db50897c8e2362c71571cfaeb9761ee40727f683f1793cda9df16"
        , Maybe.map (abiEncode << E.address) lad_
        ]
    }


logNewCupDecoder : Decoder LogNewCup
logNewCupDecoder =
    Decode.succeed LogNewCup
        |> custom (topic 1 D.address)
        |> custom (data 0 (D.staticBytes 32))



-- LogNote(bytes4,address,bytes32,bytes32,uint256,bytes) event


type alias LogNote =
    { sig : Hex
    , guy : Address
    , foo : Hex
    , bar : Hex
    , wad : BigInt
    , fax : Hex
    }


logNoteEvent : Address -> Maybe Hex -> Maybe Address -> Maybe Hex -> Maybe Hex -> LogFilter
logNoteEvent contractAddress sig_ guy_ foo_ bar_ =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics =
        [ Just <| U.unsafeToHex "644843f351d3fba4abcd60109eaff9f54bac8fb8ccf0bab941009c21df21cf31"
        , Maybe.map (abiEncode << E.staticBytes 4) sig_
        , Maybe.map (abiEncode << E.address) guy_
        , Maybe.map (abiEncode << E.staticBytes 32) foo_
        , Maybe.map (abiEncode << E.staticBytes 32) bar_
        ]
    }


logNoteDecoder : Decoder LogNote
logNoteDecoder =
    Decode.succeed LogNote
        |> custom (topic 1 (D.staticBytes 4))
        |> custom (topic 2 D.address)
        |> custom (topic 3 (D.staticBytes 32))
        |> custom (topic 4 (D.staticBytes 32))
        |> custom (data 0 D.uint)
        |> custom (data 1 D.dynamicBytes)



-- LogSetAuthority(address) event


type alias LogSetAuthority =
    { authority : Address }


logSetAuthorityEvent : Address -> Maybe Address -> LogFilter
logSetAuthorityEvent contractAddress authority_ =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics =
        [ Just <| U.unsafeToHex "1abebea81bfa2637f28358c371278fb15ede7ea8dd28d2e03b112ff6d936ada4"
        , Maybe.map (abiEncode << E.address) authority_
        ]
    }


logSetAuthorityDecoder : Decoder LogSetAuthority
logSetAuthorityDecoder =
    Decode.succeed LogSetAuthority
        |> custom (topic 1 D.address)



-- LogSetOwner(address) event


type alias LogSetOwner =
    { owner : Address }


logSetOwnerEvent : Address -> Maybe Address -> LogFilter
logSetOwnerEvent contractAddress owner_ =
    { fromBlock = LatestBlock
    , toBlock = LatestBlock
    , address = contractAddress
    , topics =
        [ Just <| U.unsafeToHex "ce241d7ca1f669fee44b6fc00b8eba2df3bb514eed0f6f668f8f89096e81ed94"
        , Maybe.map (abiEncode << E.address) owner_
        ]
    }


logSetOwnerDecoder : Decoder LogSetOwner
logSetOwnerDecoder =
    Decode.succeed LogSetOwner
        |> custom (topic 1 D.address)
