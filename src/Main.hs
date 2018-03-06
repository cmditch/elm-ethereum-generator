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
    "0.1.2"


main :: IO ()
main =  do
    setLocaleEncoding utf8
    args <- Env.getArgs
    readConfig =<< Opt.handleParseResult (parse args)


readConfig :: Config -> IO ()
readConfig config = do
    let writeTheFile path content = BS.writeFile path $ Text.encodeUtf8 content
    let outputPath = _output config
    rawABI <- Text.decodeUtf8 <$> BS.readFile (_input config)
    decodedABI <- eitherDecode <$> BS.readFile (_input config) :: IO (Either String ContractABI)
    case decodedABI of
        Left err          -> putStrLn err
        Right contractAbi -> writeTheFile outputPath $ generate (rawABI, contractAbi, outputPath)


{- Parse -}

data Config = Config
    { _input  :: FilePath
    , _output :: FilePath
    }

parse :: [String] -> Opt.ParserResult Config
parse args =
  Opt.execParserPure preferences parser args


flags :: Parser Config
flags = Config
    <$> input
    <*> output


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


helpInfo :: Opt.InfoMod Config
helpInfo =
  mconcat
      [ Opt.fullDesc
      , Opt.headerDoc $ Just top
      , Opt.progDesc "Generate elm-web3 contract interfaces from it's ABI."
      , Opt.footerDoc (Just examples)
      ]
    where
      top =
          PP.vcat [ PP.text "elm-web3-contract v" PP.<> PP.text version ]

      examples =
          linesToDoc
          [ "Examples:"
          , "  elm-web3-contract StandardToken.sol ERC20.elm"
          ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lineList =
     PP.vcat (map PP.text lineList)
