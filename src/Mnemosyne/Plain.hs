{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mnemosyne.Plain
  ( prettyQuote
  ) where

import Mnemosyne.Type

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)

instance Pretty TextLoc where
  pretty = pretty . displayTextLoc

instance Pretty Quote where
  pretty (Quote a s l q) =
    let loc = maybe mempty ((comma <+>) . pretty) l
     in vcat (map pretty q) <> hardline <>
        indent 2 (parens $ pretty a <> colon <+> pretty s <> loc)

prettyQuote :: Quote -> Text
prettyQuote =
  renderStrict .
  layoutPretty defaultLayoutOptions .
  pretty
