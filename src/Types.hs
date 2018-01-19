{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text.Lazy (Text)
import           Utils          (toLowerFirst)


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

    | AFallback     { falPayable         :: Bool
                    , falStateMutability :: Text
                    }
    deriving (Show, Ord, Eq)



$(deriveJSON (defaultOptions {
    sumEncoding = defaultTaggedObject { tagFieldName = "type" }
  , constructorTagModifier = toLowerFirst . drop 1
  , fieldLabelModifier = toLowerFirst . drop 3 })
    ''Declaration)


newtype ContractABI = ContractABI [Declaration]
    deriving (Eq, Ord, Show)


instance FromJSON ContractABI where
    parseJSON = fmap ContractABI . parseJSON
