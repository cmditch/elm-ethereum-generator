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
import qualified Generator.Converters    as C
import qualified Generator.ElmLang       as EL
import qualified Generator.Templates     as T
import           Types
import           Utils                   (indent, paramAlphabet)


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
        middle = concatMap (<> ["\n\n"]) (genBody <$> declarations)
        end = T.contractDeployFunc


genBody :: Declaration -> [Text]
genBody func@DFunction { funName, funInputs, funOutputs } =
    genTypeSig func <> fDeclare <> (indent 1 <$> funcBody)
    where
        renamedInputsArgs =
            C.renameInputs funInputs

        inputParamNames =
            case funInputs of
                [] -> ""
                _  -> " " <> Text.intercalate " " (funArgName <$> renamedInputsArgs)

        fDeclare =
            [ funName <> inputParamNames <> " =" ]

        decoderBlock =
            "decoder =" : (indent 1 <$> complexDecoder funOutputs)

        method =
            "Just \"" <> methodName func <> "\""

        inputEncoders =
            Text.intercalate ", " (inputEncoder <$> renamedInputsArgs)

        inputEncoder FunctionArg { funArgName, funArgType } =
            C.getElmEncoder funArgType <> " " <>  funArgName

        params =
            case funInputs of
                [] -> "[]"
                _  -> "List.map Encode.string [ " <> inputEncoders <> " ]"

        paramRecord =
            T.paramRecord method params

        funcBody =
            case funOutputs of
                []  -> paramRecord "Decode.succeed ()"
                [x] -> paramRecord (C.getElmDecoder $ funArgType x)
                xs  -> EL.wrapInLet (complexDecoder xs) (paramRecord "decoder")


genBody DConstructor { conInputs } = ["type alias Constructor = " <> typeSig]
    where
        fields = toType <$> conInputs
        typeSig = case length conInputs of
            x | x == 0 -> "()"
              | x <= 2 -> EL.singleLineRecord fields
              | otherwise -> EL.multiLineRecord fields

genBody _ = []


-- | Generate Elm type signatures for solidity declaration (funcs, events, constructor)
genTypeSig :: Declaration -> [Text]
genTypeSig DFunction { funName, funInputs, funOutputs } = [typeSig]
    where
        typeSig = funName <> " : " <> Text.intercalate " -> " (inputs <> outputs)

        inputs = case funInputs of
            [] -> []
            xs -> C.typeCast . funArgType <$> xs

        outputs = ["Contract.Params " <> o]
            where
                o = case funOutputs of
                    []  -> "()"
                    [x] -> C.typeCast $ funArgType x
                    xs  -> EL.singleLineRecord (outputRecord <$> indexed xs)

        outputRecord (n, FunctionArg { funArgName, funArgType }) = case funArgName of
            -- if output var is unNamed, use v0, v1, ...
            ""    -> "v" <> textInt <> " : " <> C.typeCast funArgType
            oName -> oName <> " : " <> C.typeCast funArgType
            where
              textInt = Text.pack $ show n

genTypeSig _                         = []


-- | Generates decode pipline for multi-value return objects
complexDecoder :: [FunctionArg] -> [Text]
complexDecoder outputs =
    let
        varNames =
            funArgName <$> C.renameOutputs outputs

        decoderFunction =
            "decode (\\"
            <> Text.intercalate " " varNames
            <> " -> { "
            <> Text.intercalate ", " ((\v -> v <> " = " <> v) <$> varNames)
            <> " })"
            : []

        formattedForPipleline =
            zip (fVarDecoderNames outputs) (C.getElmDecoder . funArgType <$> outputs)

        toPipeLineText (name, decoder) =
            "|> required \"" <> name <> "\" " <> decoder

        decoderPipline =
            toPipeLineText <$> formattedForPipleline
    in
        ["decoder ="] <> (indent 1 <$> decoderFunction) <> (indent 2 <$> decoderPipline)


{- HELPERS -}

-- | ["0", "1", ...] or ["userAddress", "voteCount"]
-- | Corresponds to how web3.js names fields in return objects
fVarDecoderNames :: [FunctionArg] -> [Text]
fVarDecoderNames funcs =
    rename <$> indexed (funArgName <$> funcs)
    where
        rename (index, "")   = Text.pack $ show index
        rename (index, name) = name


-- |    "transfer(address,uint256)"
methodName :: Declaration -> Text
methodName DFunction { funName, funInputs } =
    funName
    <> "("
    <> Text.intercalate "," (funArgType <$> funInputs)
    <> ")"


toType :: FunctionArg -> Text
toType FunctionArg { funArgName, funArgType } =
    funArgName <> " : " <> C.typeCast funArgType
