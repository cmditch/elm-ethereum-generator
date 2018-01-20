{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Converters where

import           Data.List.Index (indexed)
import           Data.Monoid     ((<>))
import           Data.Text.Lazy  (Text)
import qualified Data.Text.Lazy  as Text
import           Types
import           Utils           (paramAlphabet)


-- | Convert Solidity type in ABI to elm-web3 type
typeCast :: Text -> Text
typeCast "address"  = "Address"
typeCast "bool"     = "Bool"
typeCast "string"   = "String"
typeCast tipe | Text.isPrefixOf "int" tipe  = "BigInt"
              | Text.isPrefixOf "uint" tipe = "BigInt"
              | otherwise = tipe <> "-ERROR!"


-- | Get elm decoder for solidity type
getDecoder :: Text -> Text
getDecoder "Address" = "D.addressDecoder"
getDecoder "Bool"    = "D.bool"
getDecoder "String"  = "D.string"
getDecoder "BigInt"  = "D.bigIntDecoder"
getDecoder v         = v <> "-ERROR!"


-- | Get elm enocder for solidity type
getEncoder :: Text -> Text
getEncoder "Address" = "E.encodeAddress"
getEncoder "Bool"    = "E.bool"
getEncoder "String"  = "E.string"
getEncoder "BigInt"  = "E.encodeBigInt"
getEncoder v         = v <> "-ERROR!"


inputEncoder :: Arg -> Text
inputEncoder Arg { nameAsInput, encoder } =
    encoder <> " " <>  nameAsInput


-- |    "transfer(address,uint256)"
methodName :: Declaration -> Text
methodName DFunction { funName, funInputs } =
    "\""
    <> funName
    <> "("
    <> Text.intercalate "," (funArgType <$> funInputs)
    <> ")\""
methodName _ = ""


-- |   "name : String"
outputRecord :: Arg -> Text
outputRecord Arg { nameAsOutput, elmType } =
    nameAsOutput <> " : " <> elmType


-- | The below functions/class helps normalize data for unnamed inputs/outputs
data Arg = Arg  { elmType      :: Text
                , nameAsInput  :: Text
                , nameAsOutput :: Text
                , web3Field    :: Text
                , decoder      :: Text
                , encoder      :: Text
                } deriving (Show, Eq, Ord)

class NormalizedArgs a where
    normalize  :: a -> [Arg]


instance NormalizedArgs [FunctionArg] where
    normalize args = map (rename . funcTuple) (indexed args)


instance NormalizedArgs [EventArg] where
    normalize args = map (rename . eventTuple) (indexed args)


funcTuple :: (Int, FunctionArg) -> (Int, (Text, Text))
funcTuple (i, FunctionArg { funArgName, funArgType }) = (i, (funArgName, funArgType))

eventTuple :: (Int, EventArg) -> (Int, (Text, Text))
eventTuple (i, EventArg { eveArgName, eveArgType }) = (i, (eveArgName, eveArgType))

rename :: (Int, (Text, Text)) -> Arg
rename (index, (argName, argType)) = case argName of
    "" ->  Arg type' nInput nOutput indexT decoder encoder
    _  ->  Arg type' argName argName argName decoder encoder
    where
        indexT  = Text.pack (show index)
        type'   = typeCast argType
        nInput  = alphabetInt index
        nOutput = "v" <> indexT
        decoder = getDecoder type'
        encoder = getEncoder type'


alphabetInt :: Int -> Text
alphabetInt i =
    Text.singleton $ paramAlphabet !! i
