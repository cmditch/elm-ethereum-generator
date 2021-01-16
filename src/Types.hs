{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Monad      (void)
import           Data.Aeson         (FromJSON (parseJSON), ToJSON(toJSON), Options (constructorTagModifier, fieldLabelModifier, sumEncoding),
                                     SumEncoding (TaggedObject), Value (String), defaultOptions)
import           Data.Aeson.Types   (modifyFailure, typeMismatch)
import           Data.Aeson.TH      (deriveJSON)
import           Data.Text          (Text)
import           Text.Parsec        (ParseError, char, choice, digit, eof,
                                     lookAhead, many1, manyTill, optionMaybe,
                                     parse, string, try, (<|>))
import           Text.Parsec.Text   (Parser)
import Data.String                  (fromString)
import           Utils              (toLowerFirst)


-- | Solidity types and parsers

data SolidityType =
    SolidityBool
  | SolidityAddress
  | SolidityUint Int
  | SolidityInt Int
  | SolidityString
  | SolidityBytesN Int
  | SolidityBytes
  | SolidityFixedArray Int SolidityType
  | SolidityArray SolidityType
    deriving (Eq, Show, Ord)

-- need ToJSON to satisfy TH
instance ToJSON SolidityType where
    toJSON _ = fromString "not needed"

instance FromJSON SolidityType where
    parseJSON (String v) =
        case parseSolidityType v of
            Right t  -> pure t
            Left err -> fail $ "unrecognized JSON ABI Solidity type: " <> show v
    parseJSON invalid =
            modifyFailure ("parsing SolidityType failed, " <>) $ (typeMismatch "String" invalid)

numberParser :: Parser Int
numberParser = read <$> many1 digit

parseUint :: Parser SolidityType
parseUint = do
  string "uint"
  SolidityUint <$> numberParser

parseInt :: Parser SolidityType
parseInt = do
  string "int"
  SolidityInt <$> numberParser

parseBool :: Parser SolidityType
parseBool = string "bool" >> return SolidityBool

parseString :: Parser SolidityType
parseString = string "string" >> return SolidityString

parseBytes :: Parser SolidityType
parseBytes = do
  string "bytes"
  mn <- optionMaybe numberParser
  pure $ maybe SolidityBytes SolidityBytesN mn

parseAddress :: Parser SolidityType
parseAddress = string "address" >> return SolidityAddress

solidityBasicTypeParser :: Parser SolidityType
solidityBasicTypeParser =
    choice [ try parseUint
           , try parseInt
           , try parseAddress
           , try parseBool
           , try parseString
           , parseBytes
           ]

expectEnd :: SolidityType -> Parser SolidityType
expectEnd t =
    eof *> return t

parseFixedArray :: SolidityType -> Parser SolidityType
parseFixedArray t = do
    char '['
    n <- numberParser
    char ']'
    let t' = SolidityFixedArray n t
    parseArrays t' <|> expectEnd t'

parseArray :: SolidityType -> Parser SolidityType
parseArray t = do
    string "[]"
    let t' = SolidityArray t
    parseArrays t' <|> expectEnd t'

parseArrays :: SolidityType -> Parser SolidityType
parseArrays t =
        try (parseArray t)
    <|> try (parseFixedArray t)
    <|> expectEnd t

solidityTypeParser :: Parser SolidityType
solidityTypeParser = do
    t <- solidityBasicTypeParser
    parseArrays t <|> expectEnd t

parseSolidityType :: Text -> Either ParseError SolidityType
parseSolidityType = parse solidityTypeParser "Solidity"



-- | ABI Types and JSON parsers

data ConstructorArg = ConstructorArg
    { conArgName :: !Text
    , conArgType :: !SolidityType
    , conArgInternalType :: !Text
    } deriving (Show, Eq, Ord)


$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''ConstructorArg)


data FunctionArg = FunctionArg
    { funArgName :: !Text
    , funArgType :: !SolidityType
    , funArgInternalType :: !Text
    } deriving (Show, Eq, Ord)


$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''FunctionArg)


data EventArg = EventArg
    { eveArgName    :: !Text
    , eveArgType    :: !SolidityType
    , eveArgIndexed :: !Bool
    } deriving (Show, Eq, Ord)


$(deriveJSON
    (defaultOptions {fieldLabelModifier = toLowerFirst . drop 6})
    ''EventArg)


data Declaration
    = DConstructor  { conInputs          :: ![ConstructorArg]
                    , conStateMutability :: !Text
                    }

    | DFunction     { funName            :: !Text
                    , funInputs          :: ![FunctionArg]
                    , funOutputs         :: ![FunctionArg]
                    , funStateMutability :: !Text
                    }

    | DEvent        { eveName            :: !Text
                    , eveInputs          :: ![EventArg]
                    , eveAnonymous       :: !Bool
                    }

    | DFallback     { falStateMutability :: !Text
                    }
    deriving (Show, Ord, Eq)



$(deriveJSON (defaultOptions {
    sumEncoding = TaggedObject "type" "contents"
  , constructorTagModifier = toLowerFirst . drop 1
  , fieldLabelModifier = toLowerFirst . drop 3 })
    ''Declaration)


newtype ContractABI = ContractABI [Declaration]
    deriving (Eq, Ord, Show)


instance FromJSON ContractABI where
    parseJSON = fmap ContractABI . parseJSON

