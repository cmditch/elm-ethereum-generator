{-# LANGUAGE OverloadedStrings #-}

module Generator.ElmLang
    ( wrapInLet
    , multiLineRecord
    , singleLineRecord
    ) where

import           Data.Monoid    ((<>))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Utils          (indent)


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


-- |
-- |     { a : String
-- |     , b : Int
-- |     }
-- |
multiLineRecord :: [Text] -> Text
multiLineRecord fields =
    "\n    { " <> Text.intercalate "\n    , " fields <> "\n    }"


-- |    { a : String, b : Int }
singleLineRecord :: [Text] -> Text
singleLineRecord field =
    "{ " <> Text.intercalate ", " field <> " }"
