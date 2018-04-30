{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator (generate) where

import qualified Data.List            as List
import           Data.Monoid          ((<>))
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as Text
import           Generator.Converters (Arg (..))
import qualified Generator.Converters as C
import qualified Generator.ElmLang    as EL
import qualified Generator.Templates  as T
import           Types
import qualified Utils                as U



generate :: (ContractABI, FilePath) -> Text
generate (ContractABI declarations, moduleName) =
    Text.intercalate "\n" (name <> imports <> methodsAndEvents)
    where
        name = T.moduleName $ U.getFileName moduleName
        imports = T.imports
        methodsAndEvents = concatMap (<> ["\n"]) (declarationBody <$> List.sort declarations)



-- | Generate function bodies for each declaration type

{- FUNCTION -}

declarationBody :: Declaration -> [Text]
declarationBody func@DFunction { funName, funInputs, funOutputs } =
    (funcTypeSig func)
        <> fDeclare
        <> (U.indent 1 <$> funcBody)
        <> outputTypeAlias
        <> outputDecoder
    where
        inputs = C.normalize funInputs

        inputParamNames =
            case funInputs of
                [] -> " contractAddress"
                _  -> " contractAddress " <> Text.intercalate " " (nameAsInput <$> inputs)

        fDeclare = [ funName <> inputParamNames <> " =" ]

        inputEncoders =
            Text.intercalate ", " (C.inputEncoder <$> inputs)

        normalizedOutputs =
            C.normalize funOutputs

        paramRecord :: Text -> [Text]
        paramRecord = T.callBuilder
            (C.methodName func)
            (EL.wrapArray inputEncoders)

        funcBody =
            case normalizedOutputs of
                []  -> paramRecord "Decode.succeed ()"
                [x] -> paramRecord $ "toElmDecoder " <> decoder x
                _   -> paramRecord $ funName <> "Decoder"

        outputTypeAlias =
            returnDataTypeAlias (U.textUpperFirst funName) normalizedOutputs

        outputDecoder =
            typeAliasDecoder funName normalizedOutputs

{- EVENTS -}

declarationBody event@DEvent{} = concatMap (<> ["\n"])
    [ EL.comment $ eveName event <> " event"
    , eventLogFilter event
    , eventDecoder event
    , eventReturnType event
    ]

declarationBody _ = []



-- | Generate Elm type signatures for solidity declaration (funcs, events, constructor)
funcTypeSig :: Declaration -> [Text]
funcTypeSig DFunction { funName, funInputs, funOutputs } = [ typeSig ]
    where
        typeSig = funName <> " : Address -> " <> Text.intercalate " -> " (inputs <> outputs)

        inputs = elmType <$> C.normalize funInputs

        outputs = ["Call " <> o]
            where
                o = case C.normalize funOutputs of
                    []  -> "()"
                    [x] -> elmType x
                    _   -> U.textUpperFirst funName


-- | Generates a type alias which represents the return value of an Ethereum event
eventReturnType :: Declaration -> [Text]
eventReturnType DEvent { eveName, eveInputs } =
    returnDataTypeAlias (U.textUpperFirst eveName) (C.normalize eveInputs)


-- | Generates an events default LogFilter helper Function
eventLogFilter :: Declaration -> [Text]
eventLogFilter event@DEvent { eveName, eveInputs } = [sig, declaration, body]
    where
        indexedTopics = filter (\arg -> isIndexed arg) (C.normalize eveInputs)

        typeSigHelper x = "Maybe " <> elmType x <> " ->"

        sig =
            case indexedTopics of
                [] -> U.textLowerFirst eveName <> "Event : Address -> LogFilter"
                _  -> U.textLowerFirst eveName <> "Event : Address -> " <> (Text.unwords $ typeSigHelper <$> indexedTopics) <> " LogFilter"

        declaration =
            case indexedTopics of
                [] -> U.textLowerFirst eveName <> "Event contractAddress = "
                _  -> U.textLowerFirst eveName <> "Event contractAddress " <> (Text.intercalate " " (nameAsInput <$> indexedTopics)) <> " = "

        body = Text.unlines $ U.indent 1 <$> (T.logFilterBuilder $ topicsBuilder (C.methodName event) indexedTopics)


-- | Generates the "topics" field within a Logfilter
-- | Comprised of a list of Maybe Strings
topicsBuilder :: Text -> [Arg] -> Text
topicsBuilder sig args =
    let
        defaultTopic =
            "Just <| keccak256 " <> sig

        multiTopic xs =
            defaultTopic : (makeTopic <$> xs)

        makeTopic arg =
            "Maybe.map (Evm.encode << " <> encoding arg <> ") " <> nameAsInput arg
    in
        case args of
            [] -> EL.wrapArray defaultTopic
            xs -> EL.multiLineArray $ multiTopic xs


{-|  someEventDecoder : Decoder SomeEvent
     someEventDecoder =
         decode SomeEvent
            |> custom (topic 1 uint)
            |> custom (data 0 address)
-}
eventDecoder :: Declaration -> [Text]
eventDecoder DEvent { eveName, eveInputs } = [sig, declaration] <> body
    where
        sig = U.textLowerFirst eveName <> "Decoder : Decoder " <> U.textUpperFirst eveName

        declaration = U.textLowerFirst eveName <> "Decoder = "

        body =
            map (U.indent 1)
            ([ "decode " <> U.textUpperFirst eveName ] <> eventDecoderHelper (1,0) (C.normalize eveInputs) [])

        toPipeline tipe n arg = U.indent 1 $
            "|> custom ("
                <> tipe <> " "
                <> (Text.pack $ show n)
                <> " "
                <> decoder arg
                <> ")"

        eventDecoderHelper :: (Int, Int) -> [Arg] -> [Text] -> [Text]
        eventDecoderHelper (topicIndex, dataIndex) args accum =
            case args of
                [] ->
                    reverse accum
                (x:xs) ->
                    case isIndexed x of
                        True ->
                            eventDecoderHelper (topicIndex + 1, dataIndex) xs (toPipeline "topic" topicIndex x : accum)
                        False ->
                            eventDecoderHelper (topicIndex, dataIndex + 1) xs (toPipeline "data" dataIndex x : accum)


-- | Generate type alias if multi-data output
-- | TODO get rid of newlines by outputting declarationBody properly (see how events are being output)
returnDataTypeAlias :: Text -> [Arg] -> [Text]
returnDataTypeAlias name outputs =
    case outputs of
        []  -> []
        [_] -> []
        xs  -> [ "\n\ntype alias " <> name <> " ="
               <> EL.multiLineRecord (C.outputRecord <$> xs)
               ]


-- | Generates decode pipline for multi-value return objects
typeAliasDecoder :: Text -> [Arg] -> [Text]
typeAliasDecoder name outputs =
    let
        decoderPipline = toPipeLineText <$> outputs

        toPipeLineText Arg { decoder } =
            "|> andMap " <> decoder
    in
    case outputs of
        []  -> []
        [_] -> []
        _   ->  [ "\n\n" <> name <> "Decoder : Decoder " <>  U.textUpperFirst name ]
                <> [ name <> "Decoder =" ]
                <> [ U.indent 1 ("evmDecode " <> U.textUpperFirst name) ]
                <> ( U.indent 2 <$> decoderPipline )
                <> [ U.indent 2 "|> toElmDecoder" ]

