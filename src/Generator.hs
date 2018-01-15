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
import           Data.List.Index         (indexed)
import           Data.Monoid             ((<>))
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy.IO       as Text
import           Internal


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




--- Generators

{-
  Generate elm-web3 contract from raw and decoded ABI
-}
generateAll :: (Text, ContractABI) -> Text
generateAll (rawABI, ContractABI declarations) = base <> funcs <> events
  where
    base =
      Text.concat
        [ generateModuleName "Test"
        , generateImports
        , generateABI rawABI
        ]

    funcs =
      Text.intercalate "\n\n" (generateTypeSig <$> declarations)

    events = ""


{-
  Declare module/contract name
-}
generateModuleName :: Text -> Text
generateModuleName name =
  "module " <> name <> " exposing (..)"


{-
  Declare imports
-}
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


{-
  Declare Abi value
-}
generateABI :: Text -> Text
generateABI rawABI = Text.intercalate "\n"
  [ "abi_ : Abi"
  , "abi_ ="
  , "    Abi"
  , "    \"\"\"" <> minify rawABI <> "\"\"\""
  , "\n\n"
  ]


{-
  Generate Elm type signatures for solidity declaration (funcs, events, constructor)
-}
generateTypeSig :: Declaration -> Text
generateTypeSig func@DFunction{} = typeSig
  where
    typeSig =
      funName func
      <> " : "
      <> Text.intercalate " -> " (inputs <> outputs)

    inputs =
      case funInputs func of
        [] -> []
        xs -> typeCast . funArgType <$> xs

    outputs = ["Contract.Params " <> o]
      where
        o = case funOutputs func of
            []  -> "()"
            [x] -> typeCast $ funArgType x
            xs  -> wrapInBrackets $ Text.intercalate ", " (outputRecord <$> indexed xs)

    outputRecord (n, o) =
      case funArgName o of
        -- if output is unNamed, uses var0, var1, ...
        ""    -> "var" <> textInt <> " : " <> typeCast (funArgType o)
        oName -> oName <> " : " <> typeCast (funArgType o)
      where
        textInt = Text.pack $ show n

generateTypeSig ctor@DConstructor{} = typeSig
  where
    typeSig = "type alias Constructor ="
              <>"\n    { "
              <> Text.intercalate "\n    , " fields
              <> "\n    }"

    fields = multiLineTypeSig <$> conInputs ctor

generateTypeSig _                 = ""


multiLineTypeSig :: FunctionArg -> Text
multiLineTypeSig FunctionArg { funArgName = name, funArgType = tipe } =
  name <> " : " <> typeCast tipe


-- Helpers

{-
  Convert Solidity type in ABI to elm-web3 type
-}
typeCast :: Text -> Text
typeCast "string" = "String"
typeCast "address" = "Address"
typeCast tipe | Text.isPrefixOf "int" tipe = "BigInt"
              | Text.isPrefixOf "uint" tipe = "BigInt"
              | otherwise = tipe <> "-ERROR!"


wrapInBrackets :: Text -> Text
wrapInBrackets t =
  "{ " <> t <> " }"
