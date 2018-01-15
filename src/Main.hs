{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Generator            (readJSON)
import           GHC.IO.Encoding      (setLocaleEncoding, utf8)
import qualified System.Environment   as Env


version :: String
version =
    "0.1.0"


main :: IO ()
main =  do
    setLocaleEncoding utf8
    allArgs <- Env.getArgs
    case allArgs of
        [] ->
          readJSON "abi.json"
          -- putStrLn "Give me something to work with here... A file name?"

        ["--version"] ->
          putStrLn version

        ["-v"] ->
          putStrLn version

        filePath : _ ->
          readJSON filePath
