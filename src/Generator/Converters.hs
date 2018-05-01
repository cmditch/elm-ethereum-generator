{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Converters where

import           Data.List.Index (indexed)
import           Data.Monoid     ((<>))
import           Data.Text.Lazy  (Text)
import qualified Data.Text.Lazy  as Text
import           Types
import           Utils           (paramAlphabet, sanitizeName, textLowerFirst)


-- TODO: Support fixed sized array types, e.g. address[3]
-- TODO: Support all bytes sizes, e.g. bytes32 or bytes5

-- | Convert Solidity type in ABI to elm-web3 type
getElmType :: Text -> Text
getElmType "address"  = "Address"
getElmType "bool"     = "Bool"
getElmType "bytes"    = "String"
getElmType "string"   = "String"
getElmType "bytes32"   = "String"
getElmType "bytes4"   = "String"
getElmType tipe | Text.isPrefixOf "uint" tipe && Text.isSuffixOf "[]" tipe = "List BigInt"
                | Text.isPrefixOf "bool" tipe && Text.isSuffixOf "[]" tipe = "List Bool"
                | Text.isPrefixOf "address" tipe && Text.isSuffixOf "[]" tipe = "List Address"
                | Text.isPrefixOf "uint" tipe = "BigInt"
                | Text.isPrefixOf "string" tipe = "String"
                | otherwise = tipe <> "-ERROR!"


-- | Get elm decoder for solidity type
getDecoder :: Text -> Text
getDecoder "address" = "address"
getDecoder "bool"    = "bool"
getDecoder "bytes"   = "dBytes"
getDecoder "string"  = "string"
getDecoder "bytes32"  = "string"
getDecoder "bytes4"  = "string"
getDecoder tipe | Text.isPrefixOf "uint" tipe && Text.isSuffixOf "[]" tipe = "(dArray uint)"
                | Text.isPrefixOf "bool" tipe && Text.isSuffixOf "[]" tipe = "(dArray bool)"
                | Text.isPrefixOf "address" tipe && Text.isSuffixOf "[]" tipe = "(dArray address)"
                | Text.isPrefixOf "uint" tipe = "uint"
                | Text.isPrefixOf "string" tipe = "string"
                | otherwise = tipe <> "-ERROR!"


-- | Get elm enocder for solidity type
getEncodingType :: Text -> Text
getEncodingType "address"  = "AddressE"
getEncodingType "bool"     = "BoolE"
getEncodingType "bytes"    = "DBytesE"
getEncodingType "string"   = "StringE"
getEncodingType "bytes32"   = "StringE"
getEncodingType "bytes4"   = "StringE"
getEncodingType tipe | Text.isPrefixOf "uint" tipe && Text.isSuffixOf "[]" tipe = "ListE UintE"
                     | Text.isPrefixOf "bool" tipe && Text.isSuffixOf "[]" tipe = "ListE BoolE"
                     | Text.isPrefixOf "address" tipe && Text.isSuffixOf "[]" tipe = "ListE AddressE"
                     | Text.isPrefixOf "uint" tipe = "UintE"
                     | otherwise = tipe <> "-ERROR!"


-- | orders(address,bytes32) == [ AddressE a, BytesE b ]
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
    textLowerFirst t <> "Decoder"


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
rename (index, (argName, argType, isIndexed)) = case argName of
    "" ->  Arg type' nInput nOutput indexT decoder encoder isIndexed
    _  ->  Arg type' (sanitizeName argName) (sanitizeName argName) argName decoder encoder isIndexed
    where
        indexT  = Text.pack (show index)
        type'   = getElmType argType
        nInput  = alphabetInt index
        nOutput = "v" <> indexT
        decoder = getDecoder argType
        encoder = getEncodingType argType


alphabetInt :: Int -> Text
alphabetInt i =
    Text.singleton $ paramAlphabet !! i
