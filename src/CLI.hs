{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import           Data.Aeson
import           Data.Version                 (showVersion)
import           Generator                    (generate)
import           Options.Applicative
import           Types
import           GHC.IO.Encoding              (setLocaleEncoding, utf8)

import qualified Data.ByteString              as B (readFile, writeFile)
import qualified Data.Text.Encoding           as T (encodeUtf8)
import qualified Options.Applicative          as Opt
import qualified System.Environment           as Env
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Paths_elm_ethereum_generator


run :: IO ()
run = do
    setLocaleEncoding utf8
    args <- Env.getArgs
    config <- Opt.handleParseResult (parse args)
    readConfig config


readConfig :: Config -> IO ()
readConfig config = do
    decodedABI <- eitherDecodeStrict <$> B.readFile (_input config) :: IO (Either String ContractABI)
    case decodedABI of
        Left err          -> putStrLn err
        Right contractAbi -> writeTheFile outputPath $ generate (contractAbi, outputPath) isDebug
    where
        writeTheFile path content = B.writeFile path $ T.encodeUtf8 content
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


preferences :: Opt.ParserPrefs
preferences =
  Opt.prefs Opt.showHelpOnEmpty


parser :: Opt.ParserInfo Config
parser =
  Opt.info (Opt.helper <*> version <*> flags) helpInfo


flags :: Parser Config
flags = Config
    <$> input
    <*> output
    <*> debug
    -- <*> version


input :: Opt.Parser FilePath
input =
    Opt.strArgument $ Opt.metavar "INPUT"


output :: Opt.Parser FilePath
output =
    Opt.strArgument $ Opt.metavar "OUTPUT"


debug :: Opt.Parser Bool
debug =
    Opt.switch $ long "debug" PP.<> help "Logs bytecode to console during decoding"


version :: Opt.Parser (a -> a)
version = Opt.infoOption (showVersion Paths_elm_ethereum_generator.version) $
    mconcat
        [ long "version"
        , short 'v'
        , help "Displays current version"
        ]


helpInfo :: Opt.InfoMod Config
helpInfo =
  mconcat
      [ Opt.fullDesc
      , Opt.headerDoc (Just top)
      , Opt.progDesc "Generate elm-ethereum contract interfaces from it's ABI."
      , Opt.footerDoc (Just examples)
      ]
    where
      top =
          PP.vcat [ PP.text "elm-ethereum-generator v" PP.<> (PP.text $ showVersion Paths_elm_ethereum_generator.version) ]

      examples =
          linesToDoc
          [ "Examples:"
          , "  elm-ethereum-generator abis/StandardTokenAbi.json src/Contract/ERC20.elm\n"
          ]


linesToDoc :: [String] -> PP.Doc
linesToDoc lineList =
     PP.vcat (map PP.text lineList)