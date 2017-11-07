{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}

module JsonAbi (
    ContractABI(..)
  , Declaration(..)
  , FunctionArg(..)
  , EventArg(..)
  -- , signature
  -- , methodId
  -- , eventId
  ) where

import qualified Data.Text.Encoding as T
import qualified Data.Text          as T
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Aeson.TH
import Data.Aeson
import Internal

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
  = DConstructor  { conInputs :: [FunctionArg]
                  , conPayable :: Bool
                  , conStateMutability :: Text
                  }

  | DFunction     { funName      :: Text
                  , funConstant  :: Bool
                  , funInputs    :: [FunctionArg]
                  , funOutputs   :: Maybe [FunctionArg]
                  , conPayable :: Bool
                  , conStateMutability :: Text
                  }

  | DEvent        { eveName      :: Text
                  , eveInputs    :: [EventArg]
                  , eveAnonymous :: Bool
                  }

  | DFallback     { falPayable :: Bool
                  , conStateMutability :: Text
                  }
  deriving (Show, Eq, Ord)

$(deriveJSON (defaultOptions {
    sumEncoding = defaultTaggedObject { tagFieldName = "type" }
  , constructorTagModifier = toLowerFirst . drop 1
  , fieldLabelModifier = toLowerFirst . drop 3 })
    ''Declaration)

-- | Contract ABI is a list of method / event declarations
newtype ContractABI = ContractABI { unABI :: [Declaration] }
  deriving (Eq, Ord)

-- instance Show ContractABI where
--     show (ContractABI c) = T.unpack $ T.unlines $
--         [ "Contract:" ]
--         ++ foldMap showConstructor c ++
--         [ "\tEvents:" ]
--         ++ foldMap showEvent c ++
--         [ "\tMethods:" ]
--         ++ foldMap showMethod c
--
-- instance FromJSON ContractABI where
--     parseJSON = fmap ContractABI . parseJSON
--
-- instance ToJSON ContractABI where
--     toJSON (ContractABI x) = toJSON x
--
-- showConstructor :: Declaration -> [Text]
-- showConstructor x = case x of
--     DConstructor{} -> ["\tConstructor " <> signature x]
--     _ -> []
--
-- showEvent :: Declaration -> [Text]
-- showEvent x = case x of
--     DEvent{} -> ["\t\t" <> signature x]
--     _ -> []
--
-- showMethod :: Declaration -> [Text]
-- showMethod x = case x of
--     DFunction{} ->
--         ["\t\t" <> methodId x <> " " <> signature x]
--     _ -> []
