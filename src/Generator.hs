{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Generator (
    ContractABI(..)
  , Declaration(..)
  , FunctionArg(..)
  , EventArg(..)
  , readJSON
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy    as BS
import qualified Data.Char               as Char
import qualified Data.List               as List
import           Data.List.Index         (indexed)
import           Data.Monoid             ((<>))
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy.IO       as Text
import           Utils

{-| Types |-}

data FunctionArg = FunctionArg
    { funArgName :: Text
    , funArgType :: Text
    } deriving (Show, Eq, Ord)


$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''FunctionArg)

data EventArg = EventArg
    { eveArgName    :: Text
    , eveArgType    :: Text
    , eveArgIndexed :: Bool
    } deriving (Show, Eq, Ord)


$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''EventArg)


data Declaration
    = DConstructor  { conInputs          :: [FunctionArg]
                    , conPayable         :: Bool
                    , conStateMutability :: Text
                    }

    | DFunction     { funName            :: Text
                    , funConstant        :: Bool
                    , funInputs          :: [FunctionArg]
                    , funOutputs         :: [FunctionArg]
                    , funPayable         :: Bool
                    , funStateMutability :: Text
                    }

    | DEvent        { eveName      :: Text
                    , eveInputs    :: [EventArg]
                    , eveAnonymous :: Bool
                    }

    | DFallback     { falPayable         :: Bool
                    , falStateMutability :: Text
                    }
    deriving (Show, Eq, Ord)


$(deriveJSON (defaultOptions {
    sumEncoding = defaultTaggedObject { tagFieldName = "type" }
  , constructorTagModifier = toLowerFirst . drop 1
  , fieldLabelModifier = toLowerFirst . drop 3 })
    ''Declaration)


newtype ContractABI = ContractABI [Declaration]
    deriving (Eq, Ord, Show)


instance FromJSON ContractABI where
    parseJSON = fmap ContractABI . parseJSON


readJSON :: String -> IO ()
readJSON filePath = do
    rawABI <- Text.decodeUtf8 <$> BS.readFile filePath
    decodedABI <- eitherDecode <$> BS.readFile filePath :: IO (Either String ContractABI)
    case decodedABI of
        Left err          -> putStrLn err
        Right contractAbi -> Text.putStrLn $ generateAll (rawABI, contractAbi)



{-| Generators |-}


-- | Generate elm-web3 contract from raw and decoded ABI
generateAll :: (Text, ContractABI) -> Text
generateAll (rawABI, ContractABI declarations) = Text.intercalate "\n" (base <> funcs <> events)
    where
        base =
            [ generateModuleName "Test"
            , generateImports
            , generateABI rawABI
            ]

        funcs =
            concatMap (<> ["\n\n"]) (generateFunctions <$> declarations)

        events = [""]


-- | Declare module/contract name
generateModuleName :: Text -> Text
generateModuleName name =
    "module " <> name <> " exposing (..)"


-- | Declare imports
generateImports :: Text
generateImports = Text.intercalate "\n"
    [ "\n"
    , "import BigInt exposing (BigInt)"
    , "import Json.Decode as Decode exposing (Decoder, int, string, bool)"
    , "import Json.Decode.Pipeline exposing (decode, required)"
    , "import Json.Encode as Encode"
    , "import Web3.Types exposing (..)"
    , "import Web3"
    , "import Web3.Eth.Contract as Contract"
    , "import Web3.Eth as Eth"
    , "import Web3.Decoders exposing (bigIntDecoder, eventLogDecoder, addressDecoder, hexDecoder, addressToString, hexToString)"
    , "import Task exposing (Task)"
    , "\n\n"
    ]


-- | Declare Abi value
generateABI :: Text -> Text
generateABI rawABI = Text.intercalate "\n"
    [ "abi_ : Abi"
    , "abi_ ="
    , "    Abi"
    , "    \"\"\"" <> minify rawABI <> "\"\"\""
    , "\n\n"
    ]


-- | Generate Elm type signatures for solidity declaration (funcs, events, constructor)
generateTypeSig :: Declaration -> Text
generateTypeSig DFunction { funName, funInputs, funOutputs } = typeSig
    where
        typeSig = funName <> " : " <> Text.intercalate " -> " (inputs <> outputs)

        inputs = case funInputs of
            [] -> []
            xs -> typeCast . funArgType <$> xs

        outputs = ["Contract.Params " <> o]
            where
                o = case funOutputs of
                    []  -> "()"
                    [x] -> typeCast $ funArgType x
                    xs  -> singleLineRecordType (outputRecord <$> indexed xs)

        outputRecord (n, FunctionArg { funArgName, funArgType }) = case funArgName of
            -- if output is unNamed, uses var0, var1, ...
            ""    -> "v" <> textInt <> " : " <> typeCast funArgType
            oName -> oName <> " : " <> typeCast funArgType
            where
              textInt = Text.pack $ show n

generateTypeSig DConstructor { conInputs } = "type alias Constructor = " <> typeSig
    where
        fields = fArgToElmType <$> conInputs
        typeSig = case length conInputs of
            x | x == 0 -> "()"
              | x <= 2 -> singleLineRecordType fields
              | otherwise -> multiLineRecordType fields

generateTypeSig _                         = ""


generateFunctions :: Declaration -> [Text]
generateFunctions func@DFunction { funName, funInputs, funOutputs } =
    [generateTypeSig func] <> fDeclare <> (indents 1 <$> funcBody)
    where
        renamedInputsArgs =
            renameInputs funInputs

        inputParamNames =
            case funInputs of
                [] -> ""
                _  -> " " <> Text.intercalate " " (funArgName <$> renamedInputsArgs)

        fDeclare =
            [ funName <> inputParamNames <> " =" ]

        decoderBlock =
            "decoder =" : (indents 1 <$> complexDecoder funOutputs)

        methodName =
            "\"" <> elmMethodName func <> "\""

        inputEncoders =
            Text.intercalate ", " (inputEncoder <$> renamedInputsArgs)

        inputEncoder FunctionArg { funArgName, funArgType } =
            getElmEncoder funArgType <> " " <>  funArgName

        paramsField =
            case funInputs of
                [] -> "[]"
                _  -> "List.map Encode.string [ " <> inputEncoders <> " ]"

        paramReturn decoderVal =
            [ "{ abi = abi_"
            , ", gasPrice = Just (BigInt.fromInt 300000000)"
            , ", gas = Just 300000"
            , ", methodName = Just " <> methodName
            , ", data = Nothing"
            , ", params = " <> paramsField
            , ", decoder = " <> decoderVal
            , "}"
            ]

        funcBody =
            case funOutputs of
                []  -> paramReturn "Decode.succeed ()"
                [x] -> paramReturn $ getElmDecoder $ funArgType x
                xs  -> wrapInLet decoderBlock (paramReturn "decoder")

generateFunctions _ = [""]


complexDecoder :: [FunctionArg] -> [Text]
complexDecoder outputs =
    let
        varNames =
            funArgName <$> renameOutputs outputs

        decoderFunction =
            "decode (\\"
            <> Text.intercalate " " varNames
            <> " -> { "
            <> Text.intercalate ", " ((\v -> v <> " = " <> v) <$> varNames)
            <> " })"

        formattedForPipleline =
            zip (fVarDecoderNames outputs) (getElmDecoder . funArgType <$> outputs)

        toPipeLineText (name, decoder) =
            "|> required \"" <> name <> "\" " <> decoder

        decoderPipline =
            toPipeLineText <$> formattedForPipleline
    in
        [ decoderFunction ] <> map (indents 1) decoderPipline



{-|  Utils  |-}

varAlphabet =
    ['a' .. 'p'] <> "!"

-- | " someInputOrOutput : BigInt "
fArgToElmType :: FunctionArg -> Text
fArgToElmType FunctionArg { funArgName, funArgType } =
    funArgName <> " : " <> typeCast funArgType


-- | ["0", "1", ...] or ["userAddress", "voteCount"]
fVarDecoderNames :: [FunctionArg] -> [Text]
fVarDecoderNames funcs =
    rename <$> indexed (funArgName <$> funcs)
    where
        rename (index, "")   = Text.pack $ show index
        rename (index, name) = name


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
                "" -> FunctionArg (Text.singleton $ varAlphabet !! index) (typeCast funArgType)
                _ -> FunctionArg funArgName (typeCast funArgType)


-- | " transfer(address,uint256) "
elmMethodName :: Declaration -> Text
elmMethodName DFunction { funName, funInputs } =
    funName
    <> "("
    <> Text.intercalate "," (funArgType <$> funInputs)
    <> ")"


wrapInLet :: [Text] -> [Text] -> [Text]
wrapInLet theLet theIn =
    ["let"]
    <> (indents 1 <$> theLet)
    <> ["in"]
    <> (indents 1 <$> theIn)


-- | Convert Solidity type in ABI to elm-web3 type
typeCast :: Text -> Text
typeCast "address" = "Address"
typeCast "bool" = "Bool"
typeCast "string" = "String"
typeCast tipe | Text.isPrefixOf "int" tipe = "BigInt"
              | Text.isPrefixOf "uint" tipe = "BigInt"
              | otherwise = tipe <> "-ERROR!"


getElmDecoder :: Text -> Text
getElmDecoder "address" = "addressDecoder"
getElmDecoder "bool" = "bool"
getElmDecoder "string" = "string"
getElmDecoder tipe | Text.isPrefixOf "int" tipe = "bigIntDecoder"
                   | Text.isPrefixOf "uint" tipe = "bigIntDecoder"
                   | otherwise = tipe <> "-ERROR!"


getElmEncoder :: Text -> Text
getElmEncoder "Address" = "addressToString"
getElmEncoder "Bool"    = "Encode.bool"
getElmEncoder "String"  = ""
getElmEncoder "BigInt"  = "BigInt.toString"
getElmEncoder v         = v <> "-ERROR!"


{-
    { a : String
    , b : Int
    , c : Bool
    }
-}
multiLineRecordType :: [Text] -> Text
multiLineRecordType fields =
    "\n    { " <> Text.intercalate "\n    , " fields <> "\n    }"

-- | { a : String, b : Int }
singleLineRecordType :: [Text] -> Text
singleLineRecordType field =
    "{ " <> Text.intercalate ", " field <> " }"
