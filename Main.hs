{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Environment as Env
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Data.Aeson
import Control.Monad
import qualified Data.ByteString.Lazy as BS


version :: String
version =
    "0.1.0"

-- Types

data ABIObject = ABIObject
  { constant :: Bool
  , inputs :: [ABIInput]
  , name  :: String
  , outputs :: [ABIOutput]
  , payable :: Bool
  , stateMutability :: String
  , abiObjType :: String
  } deriving Show


data ABIOutput = ABIOutput
  { outputName :: String
  , outputType :: String
  } deriving Show


data ABIInput = ABIInput
  { inputName :: String
  , inputType :: String
  } deriving Show


-- Decoders

instance FromJSON ABIObject where
  parseJSON (Object v) =
    ABIObject <$> v .: "constant"
              <*> v .: "inputs"
              <*> v .: "name"
              <*> v .: "outputs"
              <*> v .: "payable"
              <*> v .: "stateMutability"
              <*> v .: "type"
  parseJSON _ = mzero



instance FromJSON ABIInput where
  parseJSON (Object v) =
    ABIInput <$> v .: "name"
             <*> v .: "type"
  parseJSON _ = mzero



instance FromJSON ABIOutput where
  parseJSON (Object v) =
    ABIOutput <$> v .: "name"
              <*> v .: "type"
  parseJSON _ = mzero


-- Meat of it

readJSON :: String -> IO ()
readJSON filePath = do
  d <- (eitherDecode <$> BS.readFile filePath) :: IO (Either String [ABIObject])
  case d of
    Left err -> putStrLn err
    Right ps -> print ps


main :: IO ()
main =
  do  setLocaleEncoding utf8
      allArgs <- Env.getArgs
      case allArgs of
        [] ->
          putStrLn "Give me something to work with here... A file name?"

        ["--version"] ->
          putStrLn version

        ["-v"] ->
          putStrLn version

        filePath : _ ->
          readJSON filePath
