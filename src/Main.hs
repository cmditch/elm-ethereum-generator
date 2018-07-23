{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Generator                    (generate)
import           GHC.IO.Encoding              (setLocaleEncoding, utf8)
import           Options.Applicative
import           Types

import qualified Data.ByteString.Lazy         as BS
import qualified Data.Text.Lazy.Encoding      as Text
import qualified Options.Applicative          as Opt
import qualified System.Environment           as Env
import qualified Text.PrettyPrint.ANSI.Leijen as PP


version :: String
version =
    "1.1.0"


main :: IO ()
main =  do
    setLocaleEncoding utf8
    args <- Env.getArgs
    readConfig =<< Opt.handleParseResult (parse args)


readConfig :: Config -> IO ()
readConfig config = do
    decodedABI <- eitherDecode <$> BS.readFile (_input config) :: IO (Either String ContractABI)
    case decodedABI of
        Left err          -> putStrLn err
        Right contractAbi -> writeTheFile outputPath $ generate (contractAbi, outputPath) isDebug
    where
        writeTheFile path content = BS.writeFile path $ Text.encodeUtf8 content
        outputPath = _output config
        isDebug = _debug config


{- Parse -}

data Config = Config
    { _input  :: FilePath
    , _output :: FilePath
    , _debug  :: Bool
    }

parse :: [String] -> Opt.ParserResult Config
parse args =
  Opt.execParserPure preferences parser args


flags :: Parser Config
flags = Config
    <$> input
    <*> output
    <*> debug


preferences :: Opt.ParserPrefs
preferences =
  Opt.prefs Opt.showHelpOnError


parser :: Opt.ParserInfo Config
parser =
  Opt.info (Opt.helper <*> flags) helpInfo


input :: Opt.Parser FilePath
input =
    Opt.strArgument $ Opt.metavar "INPUT"


output :: Opt.Parser FilePath
output =
    Opt.strArgument $ Opt.metavar "OUTPUT"


debug :: Opt.Parser Bool
debug =
    Opt.switch $ long "debug" PP.<> help "Logs bytecode to console during decoding"


helpInfo :: Opt.InfoMod Config
helpInfo =
  mconcat
      [ Opt.fullDesc
      , Opt.headerDoc $ Just top
      , Opt.progDesc "Generate elm-ethereum contract interfaces from it's ABI."
      , Opt.footerDoc (Just examples)
      ]
    where
      top =
          PP.vcat [ PP.text "elm-ethereum-generator v" PP.<> PP.text version ]

      examples =
          linesToDoc
          [ "Examples:"
          , "  elm-ethereum-generator abis/StandardTokenAbi.json src/Contract/ERC20.elm"
          ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lineList =
     PP.vcat (map PP.text lineList)
