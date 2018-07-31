{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Converters where

import           Data.List.Index     (indexed)
import           Data.Monoid         ((<>))
import           Data.Text.Lazy      (Text)
import qualified Data.Text.Lazy      as Text

import           Generator.Templates (decodeModuleName, encodeModuleName)
import           Types
import qualified Utils

-- TODO: Support fixed sized array types, e.g. address[3]
-- TODO: Support all bytes sizes, e.g. bytes32 or bytes5

-- | Convert Solidity type in ABI to elm-ethereum type
getElmType :: Text -> Text
getElmType "address"   = "Address"
getElmType "bool"      = "Bool"
getElmType "bytes"     = "String"
getElmType "string"    = "String"
getElmType tipe | Text.isPrefixOf "uint" tipe && Text.isSuffixOf "[]" tipe = "List BigInt"
                | Text.isPrefixOf "int" tipe && Text.isSuffixOf "[]" tipe = "List BigInt"
                | Text.isPrefixOf "bool" tipe && Text.isSuffixOf "[]" tipe = "List Bool"
                | Text.isPrefixOf "address" tipe && Text.isSuffixOf "[]" tipe = "List Address"
                | (Text.isPrefixOf "bytes" tipe) && (not $ Text.isSuffixOf "]" tipe) = "String" -- TODO Check for numbers on end instead of (not list)
                | Text.isPrefixOf "uint" tipe = "BigInt"
                | Text.isPrefixOf "int" tipe = "BigInt"
                | Text.isPrefixOf "string" tipe = "String"
                | otherwise = tipe <> unsupportedError


-- | Get elm decoder for solidity type
getDecoder :: Text -> Text
getDecoder "address"  = decodeModuleName <> ".address"
getDecoder "bool"     = decodeModuleName <> ".bool"
getDecoder "bytes"    = decodeModuleName <> ".dynamicBytes"
getDecoder "string"   = decodeModuleName <> ".string"
getDecoder tipe | Text.isPrefixOf "uint" tipe && Text.isSuffixOf "[]" tipe = "(" <> decodeModuleName <> ".dynamicArray " <> decodeModuleName <> ".uint)"
                | Text.isPrefixOf "int" tipe && Text.isSuffixOf "[]" tipe = "(" <> decodeModuleName <> ".dynamicArray " <> decodeModuleName <> ".int)"
                | Text.isPrefixOf "bool" tipe && Text.isSuffixOf "[]" tipe = "(" <> decodeModuleName <> ".dynamicArray " <> decodeModuleName <> ".bool)"
                | Text.isPrefixOf "address" tipe && Text.isSuffixOf "[]" tipe = "(" <> decodeModuleName <> ".dynamicArray " <> decodeModuleName <> ".address)"
                | (Text.isPrefixOf "bytes" tipe) && (not $ Text.isSuffixOf "]" tipe) = decodeModuleName <> ".staticBytes manually-enter-length-for-now-sry" -- TODO Check for numbers on end instead of (not list)
                | Text.isPrefixOf "uint" tipe = decodeModuleName <> ".uint"
                | Text.isPrefixOf "int" tipe = decodeModuleName <> ".int"
                | Text.isPrefixOf "string" tipe = decodeModuleName <> ".string"
                | otherwise = tipe <> unsupportedError


-- | Get elm enocder for solidity type
getEncodingType :: Text -> Text
getEncodingType "address"  = encodeModuleName <> ".address"
getEncodingType "bool"     = encodeModuleName <> ".bool"
-- getEncodingType "bytes"    = encodeModuleName <> "."
-- getEncodingType "string"   = encodeModuleName <> ".string"
getEncodingType tipe | (Text.isPrefixOf "bytes" tipe) && (not $ Text.isSuffixOf "]" tipe) = encodeModuleName <> ".staticBytes manually-enter-length-for-now-sry" -- TODO Check for numbers on end instead of (not list)
                    --  | Text.isPrefixOf "uint" tipe && Text.isSuffixOf "[]" tipe = "ListE UintE"
                    --  | Text.isPrefixOf "int" tipe && Text.isSuffixOf "[]" tipe = "ListE IntE"
                    --  | Text.isPrefixOf "bool" tipe && Text.isSuffixOf "[]" tipe = "ListE BoolE"
                    --  | Text.isPrefixOf "address" tipe && Text.isSuffixOf "[]" tipe = "ListE AddressE"
                     | Text.isPrefixOf "uint" tipe = encodeModuleName <> ".uint"
                     | Text.isPrefixOf "int" tipe = encodeModuleName <> ".int"
                     | otherwise = tipe <> unsupportedError


unsupportedError :: Text
unsupportedError = "-UNSUPPORTED-plz-open-github-issue"


-- | orders(address,bytes32) == [ AbiEncode.address a, AbiEncode.staticBytes b ]
callDataEncodings :: Arg -> Text
callDataEncodings Arg { nameAsInput, encoding } =
    encoding <> " " <>  nameAsInput


-- |    "transfer(address,uint256)"
methodSignature :: Declaration -> Text
methodSignature DFunction { funName, funInputs } =
    "\"" <> funName <> "("
    <> Text.intercalate "," (funArgType <$> funInputs)
    <> ")\""
methodSignature DEvent { eveName, eveInputs } =
    "\"" <> eveName <> "("
    <> Text.intercalate "," (eveArgType <$> eveInputs)
    <> ")\""
methodSignature _ = ""


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


funcTuple :: (Int, FunctionArg) -> (Int, (Text, Text, Bool))
funcTuple (i, FunctionArg { funArgName, funArgType }) = (i, (funArgName, funArgType, False))


eventTuple :: (Int, EventArg) -> (Int, (Text, Text, Bool))
eventTuple (i, EventArg { eveArgName, eveArgType, eveArgIndexed }) = (i, (eveArgName, eveArgType, eveArgIndexed))


rename :: (Int, (Text, Text, Bool)) -> Arg
rename (index, (argName, argType, isIndexed)) =
    case argName of
        "" ->  Arg type' nInput nOutput indexT decoder encoder isIndexed
        _  ->  Arg type' (Utils.sanitizeName argName) (Utils.sanitizeName argName) argName decoder encoder isIndexed
    where
        indexT  = Text.pack (show index)
        type'   = getElmType argType
        nInput  = alphabetInt index
        nOutput = "v" <> indexT
        decoder = getDecoder argType
        encoder = getEncodingType argType


alphabetInt :: Int -> Text
alphabetInt i =
    Text.singleton $ Utils.paramAlphabet !! i
