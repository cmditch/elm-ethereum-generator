{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Generator (generate) where

import           Control.Monad.State  (State, modify, get, evalState)
import           Data.Functor         (($>))
import           Data.Map             (Map)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import           Generator.Converters (Arg (..))
import           Types

import qualified Data.Map             as M
import qualified Data.Text            as Text
import qualified Data.List            as List
import qualified Generator.Converters as C
import qualified Generator.ElmLang    as EL
import qualified Generator.Templates  as T
import qualified Utils                as U


generate :: (ContractABI, FilePath) -> Bool -> Text
generate (ContractABI declarations, moduleName) isDebug = Text.unlines (nameAndExports <> imports <> methodsAndEvents)
  where
    sortedDecs = List.sort declarations
    methodsAndEvents = concat $ evalState (traverse (declarationToElm isDebug) sortedDecs) M.empty
    exportList = concat $ evalState (traverse decExports sortedDecs) M.empty
    nameAndExports = T.moduleNameAndExports (U.getFileName moduleName) exportList
    imports = T.imports


declName :: Declaration -> Text
declName DFunction {funName} = funName
declName DEvent {eveName} = eveName
declName _ = ""

declarationToElm :: Bool -> Declaration -> State (Map Text Int) [Text]
declarationToElm isDebug func = do
  name <- overloadedName $ declName func

  pure $ concatMap (<> ["\n"]) $ filter (not . null) $
    case decTypeAlias name func of
        [] ->
            [ decComment func <> decBody isDebug name func
            , decDecoder isDebug name func
            ]
        typeAlias ->
            [ decComment func <> typeAlias
            , decBody isDebug name func
            , decDecoder isDebug name func
            ]



{-

    Exports Generation

-}
decExports :: Declaration -> State (Map Text Int) [Text]
decExports DFunction { funName, funOutputs } = do
    name <- overloadedName funName

    let
        decoderName = name <> "Decoder"
        typeAlias = typeAliasName name

    pure $ case funOutputs of
        []  -> [ name ]
        [_] -> [ name ]
        _   -> [ typeAlias, name, decoderName ]

decExports DEvent { eveName } = do
    name <- overloadedName eveName

    let
        logFilterName = name <> "Event"
        decoderName = name <> "Decoder"
        typeAlias = typeAliasName name

    pure [ typeAlias, logFilterName, decoderName ]

decExports _ = pure []



{-

    Comment Generation

-}
decComment :: Declaration -> [Text]
decComment func@DFunction{} = ["-- " <> C.methodSignature func <> " function\n\n"]
decComment event@DEvent{}   = ["-- " <> C.methodSignature event <> " event\n\n"]
decComment _ = []



{-

    Type Alias Generation

-}
decTypeAlias :: Text -> Declaration -> [Text]
decTypeAlias name DEvent { eveInputs } = EL.typeAlias (typeAliasName name) (C.outputRecord <$> C.normalize eveInputs)
decTypeAlias name DFunction { funOutputs } =
    case funOutputs of
        []  -> []
        [_] -> []
        _   -> EL.typeAlias (typeAliasName name) (C.outputRecord <$> C.normalize funOutputs)
decTypeAlias _ _ = []



{-

    Body Generation

-}
decBody :: Bool -> Text -> Declaration -> [Text]
decBody isDebug name func@DFunction {funOutputs, funInputs} = sig <> declaration <> body
    where
        normalizedInputs = C.normalize funInputs

        inputParamNames =
            case funInputs of
                [] -> " contractAddress"
                _  -> " contractAddress " <> Text.intercalate " " (nameAsInput <$> normalizedInputs)

        paramRecord :: Text -> [Text]
        paramRecord = T.callBuilder isDebug
            (C.abiMethodSignature func)
            (EL.wrapArray $ Text.intercalate ", " (C.callDataEncodings <$> normalizedInputs))

        sig = funcTypeSig name funInputs funOutputs

        declaration = [ name <> inputParamNames <> " =" ]

        toElmDecoder =
            if isDebug then
                "Abi.toElmDecoderWithDebug " <> C.methodSignature func <> " "
            else
                "toElmDecoder "

        body =
            U.indent 1 <$>
                case C.normalize funOutputs of
                    []  -> paramRecord "Decode.succeed ()"
                    [x] -> paramRecord $ toElmDecoder <> decoder x
                    _   -> paramRecord $ name <> "Decoder"


decBody _ name event@DEvent {eveInputs} = sig <> declaration <> body
    where
        indexedTopics = filter isIndexed (C.normalize eveInputs)

        typeSigHelper x = "Maybe " <> elmType x <> " ->"

        sig =
            case indexedTopics of
                [] -> [ name <> "Event : Address -> LogFilter" ]
                _  -> [ name <> "Event : Address -> " <> Text.unwords (typeSigHelper <$> indexedTopics) <> " LogFilter" ]

        declaration =
            case indexedTopics of
                [] -> [ name <> "Event contractAddress = " ]
                _  -> [ name <> "Event contractAddress " <> Text.intercalate " " (nameAsInput <$> indexedTopics) <> " = " ]

        body = U.indent 1 <$> T.logFilterBuilder (topicsBuilder (C.abiMethodSignature event) indexedTopics)

decBody _ _ _ = []



{-

    Decoder Generation

-}
decDecoder :: Bool -> Text -> Declaration -> [Text]
{-  Function Call Decoder

    someCallDecoder : Decoder SomeCall
    someCallDecoder =
        abiDecode SomeEvent
            |> andMap address
            |> andMap uint
            |> toElmDecoder
-}
decDecoder isDebug name func@DFunction { funOutputs } =
    let
        sig = [name <> "Decoder : Decoder " <> typeAliasName name]

        declaration = [ name <> "Decoder =" ]

        decoderPipline = toPipeLineText <$> C.normalize funOutputs

        toPipeLineText Arg { decoder } =
            "|> andMap " <> decoder

        toElmDecoder =
            if isDebug then
                [ U.indent 2 "|> Abi.toElmDecoderWithDebug " <> C.methodSignature func ]
            else
                [ U.indent 2 "|> toElmDecoder" ]



        body = [U.indent 1 ("abiDecode " <> typeAliasName name)]
                <> ( U.indent 2 <$> decoderPipline )
                <> toElmDecoder

    in
        case funOutputs of
            []  -> []
            [_] -> []
            _   ->  sig <> declaration <> body

{-  Event Decoder

    someEventDecoder : Decoder SomeEvent
    someEventDecoder =
        decode SomeEvent
            |> custom (topic 1 uint)
            |> custom (data 0 address)
-}
decDecoder _ name DEvent { eveInputs } =
    let
        sig = [ name <> "Decoder : Decoder " <> typeAliasName name ]

        declaration = [ name <> "Decoder = " ]

        body =
            map (U.indent 1)
            ([ "Decode.succeed " <> typeAliasName name ] <> eventPipelineBuilder (1,0) (C.normalize eveInputs) [])

        toPipeline tipe n arg = U.indent 1 $
            "|> custom ("
                <> tipe <> " "
                <> Text.pack (show n)
                <> " "
                <> decoder arg
                <> ")"

        eventPipelineBuilder :: (Int, Int) -> [Arg] -> [Text] -> [Text]
        eventPipelineBuilder (topicIndex, dataIndex) args accum =
            case args of
                [] ->
                    reverse accum
                (x:xs) ->
                    if isIndexed x then
                        eventPipelineBuilder (topicIndex + 1, dataIndex) xs (toPipeline "topic" topicIndex x : accum)
                    else
                        eventPipelineBuilder (topicIndex, dataIndex + 1) xs (toPipeline "data" dataIndex x : accum)
    in
        case eveInputs of
            [] -> []
            _  -> sig <> declaration <> body

decDecoder _ _ _ = []



{-

    HELPERS

-}

overloadedName :: Text -> State (Map Text Int) Text
overloadedName name = do
  let name' = U.textLowerFirst name
  overloads <- get
  case M.lookup name' overloads of
    Nothing -> modify (M.insert name' 1) $> name'
    Just n -> modify (M.insert name' $ n + 1) $> name' <> Text.pack (show n)

-- | Generate Elm type signatures for solidity declaration (funcs, events, constructor)
funcTypeSig :: Text -> [FunctionArg] -> [FunctionArg] -> [Text]
funcTypeSig name funInputs funOutputs = [typeSig]
    where
        typeSig = name <> " : Address -> " <> Text.intercalate " -> " (inputs <> outputs)

        inputs = elmType <$> C.normalize funInputs

        outputs = ["Call " <> o]
            where
                o = case C.normalize funOutputs of
                    []  -> "()"
                    [x] -> elmType x
                    _   -> typeAliasName name

-- | Generates the "topics" field within a Logfilter
-- | Comprised of a list of Maybe Strings
topicsBuilder :: Text -> [Arg] -> Text
topicsBuilder sig args =
    let
        defaultTopic =
            "Just <| U.unsafeToHex " <> sig

        multiTopic xs =
            defaultTopic : (makeTopic <$> xs)

        makeTopic arg =
            "Maybe.map (abiEncode << " <> encoding arg <> ") " <> nameAsInput arg
    in
        case args of
            [] -> EL.wrapArray defaultTopic
            xs -> EL.multiLineArray $ multiTopic xs


--
typeAliasName :: Text -> Text
typeAliasName name  | Text.isSuffixOf "ies" name = U.textUpperFirst (Text.dropEnd 1 name) <> "y"
                    | Text.isSuffixOf "s" name = U.textUpperFirst $ Text.dropEnd 1 name
                    | otherwise = U.textUpperFirst name
