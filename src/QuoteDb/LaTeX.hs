{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuoteDb.LaTeX
  ( laTeXQuote
  , mkStandalone
  ) where

import QuoteDb.Type hiding (quote)

import Data.List (intersperse)
import Numeric.Natural
import Text.LaTeX
import Text.LaTeX.Base.Class (comm0, comm1)

instance Texy Natural where
    texy = texy . (fromIntegral :: Natural -> Int)

instance Texy Quote where
    texy (Quote a s l q) =
        let quoteL = map texy q
            authorL = texy a
            sourceL = texy s
            locationL = maybe mempty ((", " <>) . texy) l
        in quote $
           joinLines quoteL <> newline <>
           hspace_ (CustomMeasure (comm0 "fill")) <>
           parens (authorL <> ": " <> textit sourceL <> locationL)
      where
        joinLines = mconcat . intersperse newline
        parens x = between x (texy ("(" :: Text)) (texy (")" :: Text))


instance Texy TextLoc where
    texy loc =
        let following = "f." :: Text
            ffollowing = "ff." :: Text
            endash = "--" :: Text
            comma = "," :: Text
        in case loc of
               Line x -> texy x
               LineF x -> texy x <> texy following
               LineFF x -> texy x <> texy ffollowing
               LineRange x y -> texy x <> texy endash <> texy y
               Page x l -> texy x <> texy comma <> texy l

laTeXQuote :: Quote -> Text
laTeXQuote = render . (texy :: Quote -> LaTeX)

mkStandalone :: Text -> Text
mkStandalone x = render hd <> x <> render ft
  where
    hd =
        documentclass [] "article" <> usepackage [] "libertine" <>
        comm1 "begin" doc
    ft = comm1 "end" doc
    doc = texy ("document" :: Text) :: LaTeX

