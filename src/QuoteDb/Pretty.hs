{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module QuoteDb.Pretty (prettyQuote) where

import QuoteDb.Type

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text.Prettyprint.Doc.Util (reflow)

instance Pretty TextLoc where
    pretty = pretty . displayTextLoc

instance Pretty Quote where
    pretty (Quote a s l q) =
        let loc = maybe mempty ((comma <+>) . pretty) l
        in vcat (map reflow q) <> hardline <>
           indent 2 (parens $ reflow a <> colon <+> reflow s <> loc)

prettyQuote :: Quote -> Text
prettyQuote = renderStrict . layoutPretty defaultLayoutOptions . pretty
