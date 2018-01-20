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


doIt :: (Text, ContractABI) -> Text
doIt (rawABI, ContractABI declarations) = Text.intercalate "\n" fileLines
    where
        fileLines = imports <> abi <> methodsAndEvents <> contractOps
        imports = T.moduleName "Test" <> T.imports
        abi = T.abi rawABI
        methodsAndEvents = concatMap (<> ["\n\n"]) (declarationBody <$> List.sort declarations)
        contractOps = concatMap contractOperations declarations


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
        inputs = C.normalize funInputs

        inputParamNames =
            case funInputs of
                [] -> ""
                _  -> " " <> Text.intercalate " " (nameAsInput <$> inputs)

        fDeclare = [ funName <> inputParamNames <> " =" ]

        inputEncoders =
            Text.intercalate ", " (C.inputEncoder <$> inputs)

        paramRecord :: Text -> [Text]
        paramRecord = T.paramRecord
            ("Just " <> C.methodName func)
            (EL.wrapArray inputEncoders)

        funcBody =
            case C.normalize funOutputs of
                []  -> paramRecord "Decode.succeed ()"
                [x] -> paramRecord (decoder x)
                xs  -> EL.wrapInLet (decoderBlock xs) (paramRecord "decoder")

declarationBody DConstructor { conInputs } = ["type alias Constructor = " <> typeSig]
    where
        fields = C.outputRecord <$> C.normalize conInputs
        typeSig = case length conInputs of
            x | x == 0 -> "()"
              | x <= 2 -> EL.singleLineRecord fields
              | otherwise -> EL.multiLineRecord fields


{- EVENTS -}


declarationBody event@DEvent{} = concatMap (<> ["\n\n"])
    [ EL.comment $ eveName event <> " event"
    , eventSubscribe event
    , eventOnceBody event
    , eventDecodeBody event
    ]

declarationBody _ = []


eventSubscribe :: Declaration -> [Text]
eventSubscribe DEvent { eveName } =
    [ "subscribe" <> eveName <> " : ( Address, EventId ) -> Cmd msg"
    , "    Contract.subscribe abi_ \"" <> eveName <> "\""
    ]


eventOnceTypeSig :: Declaration -> [Text]
eventOnceTypeSig DEvent { eveName, eveInputs } =
    [  "once"
    <> eveName
    <> " : Contract.Params (EventLog "
    <> EL.singleLineRecord (C.outputRecord <$> C.normalize eveInputs)
    <> ")"
    ]


eventOnceBody :: Declaration -> [Text]
eventOnceBody event@DEvent { eveName, eveInputs } =
    eventOnceTypeSig event <> declare <> (indent 1 <$> body)
    where
        declare = ["once" <> eveName <> " ="]
        paramRecord = T.paramRecord ("Just " <> eveName) "[]"
        body = case C.normalize eveInputs of
                []  -> paramRecord "Decode.succeed ()"
                [x] -> paramRecord (decoder x)
                xs  -> EL.wrapInLet (decoderBlock xs) (paramRecord "decoder")



eventDecodeTypeSig :: Declaration -> [Text]
eventDecodeTypeSig DEvent { eveName, eveInputs } =
    [ "decode"
    <> eveName
    <> " : String -> Result Error (EventLog "
    <> EL.singleLineRecord (C.outputRecord <$> C.normalize eveInputs)
    <> ")"
    ]


eventDecodeBody :: Declaration -> [Text]
eventDecodeBody event@DEvent { eveName, eveInputs } =
    eventDecodeTypeSig event <> declare <> (indent 1 <$> body)
    where
        declare = ["decode" <> eveName <> " response ="]

        pipeline decoderName =
            [ "response"
            , indent 1 "|> Decode.decodeString " <> decoderName
            , indent 1 "|> Result.mapError (\\e -> Error e)"
            ]

        body = case C.normalize eveInputs of
            []  -> pipeline "(Decode.succeed ())"
            [x] -> pipeline $ decoder x
            xs  -> EL.wrapInLet (decoderBlock xs) (pipeline "decoder")



{- encodeContractABI & estimateContractGas -}

contractOperations :: Declaration -> [Text]
contractOperations DConstructor { conInputs } = concatMap (<> ["\n"])
    [comment', encodeAbi, eGas, T.contractDeployFunc]
    where
        comment' = EL.comment "Contract Helper Functions"

        inputs = C.normalize conInputs
        patternMatchRecord = Text.intercalate ", " (nameAsOutput <$> inputs)
        encodes = EL.wrapArray $ Text.intercalate ", " (encoder <$> inputs)
        params = indent 1 <$> T.paramRecord "Nothing" encodes "hexDecoder"

        eAbiTypeSig = ["encodeContractABI : Constructor -> Task Error Hex"]
        eAbideclare = ["encodeContractABI " <> patternMatchRecord ]
        eAbiBody = ["Contract.encodeContractABI"] <> params
        encodeAbi = eAbiTypeSig <> eAbideclare <> (indent 1 <$> eAbiBody)

        eGasTypeSig = ["estimateContractGas : Constructor -> Task Error Int"]
        eGasdeclare = ["estimateContractGas " <> patternMatchRecord ]
        eGasBody = ["Contract.estimateContractGas"] <> params
        eGas = eGasTypeSig <> eGasdeclare <> (indent 1 <$> eGasBody)

contractOperations _ = []


-- | Generates decode pipline for multi-value return objects
decoderBlock :: [Arg] -> [Text]
decoderBlock outputs =
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
