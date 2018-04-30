{-# LANGUAGE OverloadedStrings #-}

module Generator.ElmLang
    ( wrapInLet
    , wrapArray
    , multiLineArray
    , wrapQuotes
    , docComment
    , multiLineRecord
    , singleLineRecord
    , typeAlias
    ) where

import           Data.Monoid          ((<>))
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as Text
import qualified Generator.Converters as C
import           Utils                (indent)


-- | let
-- |     someVals
-- | in
-- |     beEvalutaed
-- |
wrapInLet :: [Text] -> [Text] -> [Text]
wrapInLet theLet theIn =
    ["let"]
    <> (indent 1 <$> theLet)
    <> ["in"]
    <> (indent 1 <$> theIn)


wrapArray :: Text -> Text
wrapArray "" = "[]"
wrapArray t  = "[ " <> t <> " ]"

-- Pushes it 8 spaces out
multiLineArray :: [Text] -> Text
multiLineArray fields =
    "\n        [ " <> Text.intercalate "\n        , " fields <> "\n        ]"


wrapQuotes :: Text -> Text
wrapQuotes t = "\"" <> t <> "\""


docComment :: Text -> [Text]
docComment t =
    ["{-| " <> t <> "\n-}"]

-- |
-- |     { a : String
-- |     , b : Int
-- |     }
-- |
-- | Pushes it 4 spaces out
multiLineRecord :: [Text] -> Text
multiLineRecord fields =
    "\n    { " <> Text.intercalate "\n    , " fields <> "\n    }"


-- |    { a : String, b : Int }
singleLineRecord :: [Text] -> Text
singleLineRecord []     = "_"
singleLineRecord [""]   = "_"  -- Text.intercalate will output empty string on empty list. Bug started at Generator.contractOperations
singleLineRecord fields = "{ " <> Text.intercalate ", " fields <> " }"


-- | Generate type alias if multi-data output
-- | TODO get rid of newlines by outputting declarationBody properly (see how events are being output)
typeAlias :: Text -> [C.Arg] -> [Text]
typeAlias name outputs =
    case outputs of
        []  -> []
        [_] -> []
        xs  -> [ "type alias " <> name <> " ="
               <> multiLineRecord (C.outputRecord <$> xs)
               ]
