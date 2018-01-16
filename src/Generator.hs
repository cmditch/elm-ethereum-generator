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
    , "import Json.Decode as Decode exposing (Decoder, int, string)"
    , "import Json.Decode.Pipeline exposing (decode, required, optional)"
    , "import Json.Encode as Encode exposing (Value)"
    , "import Web3.Types exposing (..)"
    , "import Web3"
    , "import Web3.Eth.Contract as Contract"
    , "import Web3.Eth as Eth"
    , "import Web3.Decoders exposing (..)"
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
            ""    -> "var" <> textInt <> " : " <> typeCast funArgType
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
    [generateTypeSig func] <> fDeclare <> funcBody
    where
        fDeclare =
            [funName <> paramLetters funInputs <> "="]

        decoderBlock =
            case funOutputs of
                []  -> "decoder =" : [indents 1 "Decode.succeed ()"]
                [x] -> "decoder =" : [indents 1 $ getElmDecoder $ funArgType x]
                xs  -> "decoder =" : (indents 1 <$> complexDecoder xs)

        funcBody =
            indents 1 <$> wrapInLet decoderBlock ["Nothing"]

generateFunctions _ = [""]


complexDecoder :: [FunctionArg] -> [Text]
complexDecoder outputs =
    let
        decoderPipline = toPipeLineText <$> formattedForPipleline

        decoderFunction =
            "decode (\\"
            <> (Text.intercalate " " $ fVarNames outputs)
            <> " -> { "
            <> (Text.intercalate ", " $ (\v -> v <> " = " <> v) <$> fVarNames outputs)
            <> " })"

        toPipeLineText (name, decoder) =
            "|> required \"" <> name <> "\" " <> decoder

        formattedForPipleline =
            zip (fVarDecoderNames outputs) (getElmDecoder . funArgType <$> outputs)

    in
        [ decoderFunction ] <> map (indents 1) decoderPipline



{-|  Utils  |-}


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
fVarNames :: [FunctionArg] -> [Text]
fVarNames funcs =
    rename <$> indexed (funArgName <$> funcs)
    where
        rename (index, "")   = "var" <> (Text.pack $ show index)
        rename (index, name) = name


-- | " transfer(address,uint256) "
methodName :: Declaration -> Text
methodName DFunction { funName, funInputs } =
    funName
    <> "("
    <> Text.intercalate "," (funArgType <$> funInputs)
    <> ")"


-- | Creates param names for Elm function declarations
-- | EVM only allows for 16 inputs I believe, Elm compiler will fail if inputs > 16 due to "!" safegaurd
paramLetters :: [FunctionArg] -> Text
paramLetters args =
    Text.chunksOf 1 "abcdefghijklmnop!"
        |> take (length args)
        |> Text.intercalate " "
        |> addSpaces


addSpaces :: Text -> Text
addSpaces t =
    " " <> t <> " "


wrapInLet :: [Text] -> [Text] -> [Text]
wrapInLet theLet theIn =
    ["let"]
    <> (indents 1 <$> theLet)
    <> ["in"]
    <> (indents 1 <$> theIn)


-- | Convert Solidity type in ABI to elm-web3 type
typeCast :: Text -> Text
typeCast "string" = "String"
typeCast "address" = "Address"
typeCast tipe | Text.isPrefixOf "int" tipe = "BigInt"
              | Text.isPrefixOf "uint" tipe = "BigInt"
              | otherwise = tipe <> "-ERROR!"


getElmDecoder :: Text -> Text
getElmDecoder "string" = "string"
getElmDecoder "address" = "addressDecoder"
getElmDecoder tipe | Text.isPrefixOf "int" tipe = "bigIntDecoder"
                   | Text.isPrefixOf "uint" tipe = "bigIntDecoder"
                   | otherwise = tipe <> "-ERROR!"


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
