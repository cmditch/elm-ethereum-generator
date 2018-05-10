module SaiTub
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
        , Cups
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
import Evm.Decode as Evm exposing (evmDecode, andMap, toElmDecoder, topic, data)
import Evm.Encode as Evm exposing (Encoding(..), evmEncode)


{-| "air()" function
-}
air : Address -> Call BigInt
air contractAddress =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "air()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "ask(uint256)" [ UintE wad ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "authority()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "axe()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "bid(uint256)" [ UintE wad ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "bite(bytes32)" [ StringE cup ]
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
    , data = Just <| Evm.encodeFunctionCall "cage(uint256,uint256)" [ UintE fit_, UintE jam ]
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
    , data = Just <| Evm.encodeFunctionCall "cap()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "chi()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "cupi()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
    }


{-| "cups(bytes32)" function
-}
type alias Cups =
    { lad : Address
    , ink : BigInt
    , art : BigInt
    , ire : BigInt
    }


cups : Address -> String -> Call Cups
cups contractAddress a =
    { to = Just contractAddress
    , from = Nothing
    , gas = Nothing
    , gasPrice = Nothing
    , value = Nothing
    , data = Just <| Evm.encodeFunctionCall "cups(bytes32)" [ StringE a ]
    , nonce = Nothing
    , decoder = cupsDecoder
    }


cupsDecoder : Decoder Cups
cupsDecoder =
    evmDecode Cups
        |> andMap Evm.address
        |> andMap Evm.uint
        |> andMap Evm.uint
        |> andMap Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "din()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "draw(bytes32,uint256)" [ StringE cup, UintE wad ]
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
    , data = Just <| Evm.encodeFunctionCall "drip()" []
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
    , data = Just <| Evm.encodeFunctionCall "era()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "exit(uint256)" [ UintE wad ]
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
    , data = Just <| Evm.encodeFunctionCall "fee()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "fit()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "flow()" []
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
    , data = Just <| Evm.encodeFunctionCall "free(bytes32,uint256)" [ StringE cup, UintE wad ]
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
    , data = Just <| Evm.encodeFunctionCall "gap()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "gem()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "give(bytes32,address)" [ StringE cup, AddressE guy ]
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
    , data = Just <| Evm.encodeFunctionCall "gov()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "ink(bytes32)" [ StringE cup ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "join(uint256)" [ UintE wad ]
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
    , data = Just <| Evm.encodeFunctionCall "lad(bytes32)" [ StringE cup ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "lock(bytes32,uint256)" [ StringE cup, UintE wad ]
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
    , data = Just <| Evm.encodeFunctionCall "mat()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "mold(bytes32,uint256)" [ StringE param, UintE val ]
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
    , data = Just <| Evm.encodeFunctionCall "off()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.bool
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
    , data = Just <| Evm.encodeFunctionCall "open()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.string
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
    , data = Just <| Evm.encodeFunctionCall "out()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.bool
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
    , data = Just <| Evm.encodeFunctionCall "owner()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "pep()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "per()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "pie()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "pip()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "pit()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "rap(bytes32)" [ StringE cup ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "rhi()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "rho()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "rum()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "safe(bytes32)" [ StringE cup ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.bool
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
    , data = Just <| Evm.encodeFunctionCall "sai()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "setAuthority(address)" [ AddressE authority_ ]
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
    , data = Just <| Evm.encodeFunctionCall "setOwner(address)" [ AddressE owner_ ]
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
    , data = Just <| Evm.encodeFunctionCall "setPep(address)" [ AddressE pep_ ]
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
    , data = Just <| Evm.encodeFunctionCall "setPip(address)" [ AddressE pip_ ]
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
    , data = Just <| Evm.encodeFunctionCall "setVox(address)" [ AddressE vox_ ]
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
    , data = Just <| Evm.encodeFunctionCall "shut(bytes32)" [ StringE cup ]
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
    , data = Just <| Evm.encodeFunctionCall "sin()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "skr()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "tab(bytes32)" [ StringE cup ]
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "tag()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "tap()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "tax()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.uint
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
    , data = Just <| Evm.encodeFunctionCall "turn(address)" [ AddressE tap_ ]
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
    , data = Just <| Evm.encodeFunctionCall "vox()" []
    , nonce = Nothing
    , decoder = toElmDecoder Evm.address
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
    , data = Just <| Evm.encodeFunctionCall "wipe(bytes32,uint256)" [ StringE cup, UintE wad ]
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
        , Maybe.map (evmEncode << AddressE) lad
        ]
    }


logNewCupDecoder : Decoder LogNewCup
logNewCupDecoder = 
    decode LogNewCup
        |> custom (topic 1 Evm.address)
        |> custom (data 0 Evm.string)


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
        , Maybe.map (evmEncode << StringE) sig
        , Maybe.map (evmEncode << AddressE) guy
        , Maybe.map (evmEncode << StringE) foo
        , Maybe.map (evmEncode << StringE) bar
        ]
    }


logNoteDecoder : Decoder LogNote
logNoteDecoder = 
    decode LogNote
        |> custom (topic 1 Evm.string)
        |> custom (topic 2 Evm.address)
        |> custom (topic 3 Evm.string)
        |> custom (topic 4 Evm.string)
        |> custom (data 0 Evm.uint)
        |> custom (data 1 Evm.dBytes)


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
        , Maybe.map (evmEncode << AddressE) authority
        ]
    }


logSetAuthorityDecoder : Decoder LogSetAuthority
logSetAuthorityDecoder = 
    decode LogSetAuthority
        |> custom (topic 1 Evm.address)


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
        , Maybe.map (evmEncode << AddressE) owner
        ]
    }


logSetOwnerDecoder : Decoder LogSetOwner
logSetOwnerDecoder = 
    decode LogSetOwner
        |> custom (topic 1 Evm.address)


