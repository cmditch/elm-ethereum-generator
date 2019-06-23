{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Converters where

import           Data.List.Index     (indexed)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Data.Text.Encoding  (encodeUtf8)
import           Generator.Templates (decodeModuleName, encodeModuleName)
import           Utils (tshow)
import           Types (SolidityType (..), Declaration(..), FunctionArg(..), EventArg(..))

import qualified Generator.ElmLang as EL
import qualified Crypto.Hash as Crypto
import qualified Utils
import qualified Data.Text as Text

-- TODO: Support fixed sized array types, e.g. address[3]
-- TODO: Support all bytes sizes, e.g. bytes32 or bytes5

-- | Convert Solidity type in ABI to elm-ethereum type
toElmType :: SolidityType -> Text
toElmType SolidityBool             = "Bool"
toElmType SolidityAddress          = "Address"
toElmType (SolidityUint _)         = "BigInt"
toElmType (SolidityInt _)          = "BigInt"
toElmType SolidityString           = "String"
toElmType (SolidityBytesN _)       = "Hex"
toElmType SolidityBytes            = "Hex"
toElmType (SolidityFixedArray _ t) = "List (" <> toElmType t <> ")"
toElmType (SolidityArray t)        = "List (" <> toElmType t <> ")"


-- | Get elm decoder for solidity type
toElmDecoder :: SolidityType -> Text
toElmDecoder SolidityBool             = decodeModuleName <> ".bool"
toElmDecoder SolidityAddress          = decodeModuleName <> ".address"
toElmDecoder (SolidityUint _)         = decodeModuleName <> ".uint"
toElmDecoder (SolidityInt _)          = decodeModuleName <> ".int"
toElmDecoder SolidityString           = decodeModuleName <> ".string"
toElmDecoder (SolidityBytesN n)       = "(" <> decodeModuleName <> ".staticBytes " <> tshow n <> ")"
toElmDecoder SolidityBytes            = decodeModuleName <> ".dynamicBytes"
toElmDecoder (SolidityFixedArray n t) = "(" <> decodeModuleName <> ".staticArray " <> tshow n <> " " <> toElmDecoder t <> ")"
toElmDecoder (SolidityArray t)        = "(" <> decodeModuleName <> ".dynamicArray " <> toElmDecoder t <> ")"


-- | Get elm enocder for solidity type
toElmEncoder :: SolidityType -> Text
toElmEncoder SolidityBool             = encodeModuleName <> ".bool"
toElmEncoder SolidityAddress          = encodeModuleName <> ".address"
toElmEncoder (SolidityUint _)         = encodeModuleName <> ".uint"
toElmEncoder (SolidityInt _)          = encodeModuleName <> ".int"
toElmEncoder SolidityString           = encodeModuleName <> ".string"
toElmEncoder (SolidityBytesN n)       = "(" <> encodeModuleName <> ".staticBytes " <> tshow n <> ")"
toElmEncoder SolidityBytes            = encodeModuleName <> ".bytes"
toElmEncoder (SolidityFixedArray n t) = "(" <> encodeModuleName <> ".list << List.map " <> toElmEncoder t <> ")"
toElmEncoder (SolidityArray t)        = "(" <> encodeModuleName <> ".list << List.map " <> toElmEncoder t <> ")" -- "*unsupported-input-type*"

-- | Get elm enocder for solidity type
toABI :: SolidityType -> Text
toABI SolidityBool             = "bool"
toABI SolidityAddress          = "address"
toABI (SolidityUint n)         = "uint" <> tshow n
toABI (SolidityInt n)          = "int" <> tshow n
toABI SolidityString           = "string"
toABI (SolidityBytesN n)       = "bytes" <> tshow n
toABI SolidityBytes            = "bytes"
toABI (SolidityFixedArray n t) = toABI t <> "[" <> tshow n <> "]"
toABI (SolidityArray t)        = toABI t <> "[]"


-- | orders(address,bytes32) == [ AbiEncode.address a, AbiEncode.staticBytes b ]
callDataEncodings :: Arg -> Text
callDataEncodings Arg { nameAsInput, encoding } =
    encoding <> " " <>  nameAsInput


-- |    "transfer(address,uint256)"
methodSignature :: Declaration -> Text
methodSignature DFunction { funName, funInputs } =
    funName <> "("
    <> Text.intercalate "," (toABI . funArgType <$> funInputs)
    <> ")"
methodSignature DEvent { eveName, eveInputs } =
    eveName <> "("
    <> Text.intercalate "," (toABI . eveArgType <$> eveInputs)
    <> ")"
methodSignature _ = ""


-- | "transfer(address,uint256)" -> "a9059cbb"
abiMethodSignature :: Declaration -> Text
abiMethodSignature dec@DFunction{} = EL.wrapQuotes . Text.take 8 . Text.pack . show $ decToKeccak dec
abiMethodSignature dec@DEvent{} = EL.wrapQuotes . Text.pack . show $ decToKeccak dec
abiMethodSignature _ = ""

decToKeccak :: Declaration -> Crypto.Digest Crypto.Keccak_256
decToKeccak = Crypto.hash . encodeUtf8 . methodSignature


-- |   "name : String"
outputRecord :: Arg -> Text
outputRecord Arg { nameAsOutput, elmType } =
    nameAsOutput <> " : " <> elmType


eventDecoderName :: Text -> Text
eventDecoderName t =
    Utils.textLowerFirst t <> "Decoder"


-- | The below functions/class helps normalize data for unnamed inputs/outputs

data Arg = Arg  { elmType      :: Text
                , nameAsInput  :: Text
                , nameAsOutput :: Text
                , web3Field    :: Text
                , decoder      :: Text
                , encoding     :: Text
                , isIndexed    :: Bool
                } deriving (Show, Eq, Ord)


class NormalizedArgs a where
    normalize  :: a -> [Arg]


instance NormalizedArgs [FunctionArg] where
    normalize args = map (rename . funcTuple) (indexed args)


instance NormalizedArgs [EventArg] where
    normalize args = map (rename . eventTuple) (indexed args)


funcTuple :: (Int, FunctionArg) -> (Int, (Text, SolidityType, Bool))
funcTuple (i, FunctionArg { funArgName, funArgType }) = (i, (funArgName, funArgType, False))


eventTuple :: (Int, EventArg) -> (Int, (Text, SolidityType, Bool))
eventTuple (i, EventArg { eveArgName, eveArgType, eveArgIndexed }) = (i, (eveArgName, eveArgType, eveArgIndexed))


rename :: (Int, (Text, SolidityType, Bool)) -> Arg
rename (index, (argName, argType, isIndexed)) =
    case argName of
        "" ->  Arg type' nInput nOutput indexT decoder encoder isIndexed
        _  ->  Arg type' (Utils.cleanPrependingChars argName <> "_") (Utils.cleanPrependingChars argName) argName decoder encoder isIndexed
    where
        indexT  = Text.pack (show index)
        type'   = toElmType argType
        nInput  = alphabetInt index <> "_"
        nOutput = "v" <> indexT
        decoder = toElmDecoder argType
        encoder = toElmEncoder argType


alphabetInt :: Int -> Text
alphabetInt i =
    Text.singleton $ Utils.paramAlphabet !! i
