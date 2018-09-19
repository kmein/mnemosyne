{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuoteDb.LaTeX
  ( mkLaTeXDocument
  ) where

import QuoteDb.Type hiding (quote)

import Data.Function (on)
import Data.List (groupBy, intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text (pack, splitOn)
import Numeric.Natural (Natural)
import Text.LaTeX hiding (author)
import Text.LaTeX.Base.Class (comm1)
import Text.LaTeX.Packages.Hyperref (hyperref)

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
          , comm1 "attrib" $ mconcat [authorL, ": ", textit sourceL, locationL]
          ]
    where
      joinLines = mconcat . intersperse newline

instance Texy TextLoc where
  texy = texy . Text.pack . toText
    where
      toText loc =
        case loc of
          Line x -> show x
          LineF x -> show x ++ "f."
          LineFF x -> show x ++ "ff."
          LineRange x y -> show x ++ "--" ++ show y
          Page x l -> show x ++ "," ++ toText l

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

mkLaTeXDocument :: Maybe Font -> [Quote] -> Text
mkLaTeXDocument font qs =
  render $
  mconcat
    [ preamble
    , document (tableofcontents <> foldMap mkSect (groupOn author qs)) :: LaTeX
    ]
  where
    mkSect quotes =
      case quotes of
        [] -> mempty
        (q:_) ->
          case Text.splitOn "/" (author q) of
            [a] -> section (texy a) <> foldMap texy quotes
            [_, t] -> subsection (texy t) <> foldMap texy quotes
            _ -> mempty
    preamble =
      mconcat
        [ documentclass [] "scrartcl"
        , usepackage [] "fontspec"
        , usepackage [] "attrib"
        , usepackage [] hyperref
        , comm1 "setmainfont" (texy $ fromMaybe _DEFAULT_FONT_ font)
        , comm1 "setsansfont" (texy $ fromMaybe _DEFAULT_FONT_ font)
        ]
