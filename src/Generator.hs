{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator (readJSON) where

import           Control.Monad
import           Data.Aeson              (eitherDecode)
import qualified Data.ByteString.Lazy    as BS
import qualified Data.Char               as Char
import qualified Data.List               as List
import           Data.List.Index         (indexed)
import           Data.Monoid             ((<>))
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as Text
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy.IO       as Text
import           Generator.Converters    (Arg (..))
import qualified Generator.Converters    as C
import qualified Generator.ElmLang       as EL
import qualified Generator.Templates     as T
import           Types
import           Utils                   (indent, paramAlphabet, textLowerFirst)


readJSON :: String -> IO ()
readJSON filePath = do
    rawABI <- Text.decodeUtf8 <$> BS.readFile filePath
    decodedABI <- eitherDecode <$> BS.readFile filePath :: IO (Either String ContractABI)
    case decodedABI of
        Left err          -> putStrLn err
        Right contractAbi -> Text.putStrLn $ doIt (rawABI, contractAbi)


-- | Generate elm-web3 contract from raw and decoded ABI
doIt :: (Text, ContractABI) -> Text
doIt (rawABI, ContractABI declarations) = Text.intercalate "\n" fileLines
    where
        fileLines = beginning <> middle <> end
        beginning = T.moduleName "Test" <> T.imports <> T.abi rawABI
        middle = concatMap (<> ["\n\n"]) (declarationBody <$> List.sort declarations)
        end = T.contractDeployFunc

{- FUNCTIONS -}

-- | Generate Elm type signatures for solidity declaration (funcs, events, constructor)
funcTypeSig :: Declaration -> [Text]
funcTypeSig DFunction { funName, funInputs, funOutputs } = [ typeSig ]
    where
        inputs = elmType <$> C.normalize funInputs

        typeSig = funName <> " : " <> Text.intercalate " -> " (inputs <> outputs)

        outputs = ["Contract.Params " <> o]
            where
                o = case C.normalize funOutputs of
                    []  -> "()"
                    [x] -> elmType x
                    xs  -> EL.singleLineRecord (C.outputRecord <$> xs)


declarationBody :: Declaration -> [Text]
declarationBody func@DFunction { funName, funInputs, funOutputs } =
    funcTypeSig func <> fDeclare <> (indent 1 <$> funcBody)
    where
        normalizedFunInput = C.normalize funInputs

        inputParamNames =
            case funInputs of
                [] -> ""
                _  -> " " <> Text.intercalate " " (nameAsInput <$> normalizedFunInput)

        fDeclare = [ funName <> inputParamNames <> " =" ]

        inputEncoders =
            Text.intercalate ", " (C.inputEncoder <$> normalizedFunInput)

        paramRecord = T.paramRecord
            ("Just " <> C.methodName func)
            (EL.wrapArray inputEncoders)

        funcBody =
            case C.normalize funOutputs of
                []  -> paramRecord "Decode.succeed ()"
                [x] -> paramRecord (decoder x)
                xs  -> EL.wrapInLet (funcDecoderBlock xs) (paramRecord "decoder")

genBody DConstructor { conInputs } = ["type alias Constructor = " <> typeSig]
    where
        fields = C.outputRecord <$> C.normalize conInputs
        typeSig = case length conInputs of
            x | x == 0 -> "()"
              | x <= 2 -> EL.singleLineRecord fields
              | otherwise -> EL.multiLineRecord fields

genBody event@DEvent { eveName, eveInputs } = eventOnceTypeSig event <> T.subscribe eveName
    where
        a = ""

genBody _ = []


-- | Generates decode pipline for multi-value return objects
funcDecoderBlock :: [Arg] -> [Text]
funcDecoderBlock outputs =
    let
        decoderPipline = toPipeLineText <$> outputs

        varNames = nameAsOutput <$> outputs

        decoderFunction =
            "decode (\\"
            <> Text.intercalate " " varNames
            <> " -> { "
            <> Text.intercalate ", "  ((\v -> v <> " = " <> v) . nameAsOutput <$> outputs)
            <> " })"

        toPipeLineText Arg { web3Field, decoder } =
            "|> required \"" <> web3Field <> "\" " <> decoder
    in
        ["decoder ="] <> [indent 1 decoderFunction] <> (indent 2 <$> decoderPipline)


eventOnceTypeSig :: Declaration -> [Text]
eventOnceTypeSig DEvent { eveName, eveInputs } =
    [  textLowerFirst eveName
    <> " : Contract.Params (EventLog "
    <> EL.singleLineRecord (C.outputRecord <$> C.normalize eveInputs)
    <> ")"
    ]
