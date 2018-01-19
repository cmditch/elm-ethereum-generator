{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator.Converters
    ( typeCast
    , getElmDecoder
    , getElmEncoder
    , renameInputs
    , renameOutputs
    ) where

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


-- | Get decoder for solidity type
getElmDecoder :: Text -> Text
getElmDecoder "address" = "addressDecoder"
getElmDecoder "bool"    = "D.bool"
getElmDecoder "string"  = "D.string"
getElmDecoder tipe | Text.isPrefixOf "int" tipe = "bigIntDecoder"
                   | Text.isPrefixOf "uint" tipe = "bigIntDecoder"
                   | otherwise = tipe <> "-ERROR!"


-- | Get enocder for solidity type
getElmEncoder :: Text -> Text
getElmEncoder "Address" = "D.string <| addressToString"
getElmEncoder "Bool"    = "D.string <| E.bool"
getElmEncoder "String"  = ""
getElmEncoder "BigInt"  = "D.string <| BI.toString"
getElmEncoder v         = v <> "-ERROR!"


-- | ["var0", "var1", ...] or ["userAddress", "voteCount"]
renameOutputs :: [FunctionArg] -> [FunctionArg]
renameOutputs funcs = rename <$> indexed funcs
    where
        rename (index, FunctionArg { funArgName, funArgType }) =
            case funArgName of
                "" -> FunctionArg ("v" <> Text.pack (show index)) (typeCast funArgType)
                _ -> FunctionArg funArgName (typeCast funArgType)


renameInputs :: [FunctionArg] -> [FunctionArg]
renameInputs funcs = rename <$> indexed funcs
    where
        rename (index, FunctionArg { funArgName, funArgType }) =
            case funArgName of
                "" -> FunctionArg (Text.singleton $ paramAlphabet !! index) (typeCast funArgType)
                _ -> FunctionArg funArgName (typeCast funArgType)
