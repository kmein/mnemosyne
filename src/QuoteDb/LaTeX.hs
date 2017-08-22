{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuoteDb.LaTeX
    ( mkLaTeXDocument
    ) where

import QuoteDb.Type hiding (quote)

import Data.Function (on)
import Data.List (groupBy, intersperse)
import Data.Maybe (fromMaybe)
import Numeric.Natural (Natural)
import Text.LaTeX hiding (author)
import Text.LaTeX.Base.Class (comm0, comm1)
import Text.LaTeX.Base.Pretty

_DEFAULT_FONT_ :: Text
_DEFAULT_FONT_ = "Linux Libertine O"

instance Texy Natural where
    texy = texy . (fromIntegral :: Natural -> Int)

instance Texy Quote where
    texy (Quote a s l q) =
        let quoteL = map texy q
            authorL = texy a
            sourceL = texy s
            locationL = maybe mempty ((", " <>) . texy) l
        in quote $
           mconcat
               [ joinLines quoteL
               , rightAlign
               , parens $ mconcat [authorL, ": ", textit sourceL, locationL]
               ]
      where
        joinLines = mconcat . intersperse newline
        parens x = between x (texy ("(" :: Text)) (texy (")" :: Text))
        rightAlign = newline <> hspace_ (CustomMeasure (comm0 "fill"))

instance Texy TextLoc where
    texy loc =
        case loc of
            Line x -> texy x
            LineF x -> texy x <> texy following
            LineFF x -> texy x <> texy ffollowing
            LineRange x y -> texy x <> texy endash <> texy y
            Page x l -> texy x <> texy comma <> texy l
      where
        following = "f." :: Text
        ffollowing = "ff." :: Text
        endash = "--" :: Text
        comma = "," :: Text

groupOn
    :: (Eq b)
    => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

mkLaTeXDocument :: Maybe Font -> [Quote] -> String
mkLaTeXDocument font qs =
    prettyLaTeX $
    mconcat
        [ preamble
        , document (tableofcontents <> foldMap mkSect (groupOn author qs)) :: LaTeX
        ]
  where
    mkSect quotes =
        case quotes of
            [] -> mempty
            (q:_) -> section (texy $ author q) <> foldMap texy quotes
    preamble =
        mconcat
            [ documentclass [] "article"
            , usepackage [] "fontspec"
            , comm1 "setmainfont" (texy $ fromMaybe _DEFAULT_FONT_ font)
            ]
