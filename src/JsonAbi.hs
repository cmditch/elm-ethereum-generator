{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module JsonAbi (
    ContractABI(..)
  , Declaration(..)
  , FunctionArg(..)
  , EventArg(..)
  , readJSON
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy as BS
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
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
  d <- (eitherDecode <$> BS.readFile filePath) :: IO (Either String ContractABI)
  case d of
    Left err -> putStrLn err
    Right ps -> print ps
