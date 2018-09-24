{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module QuoteDb.Type
  ( Quote(..)
  , TextLoc(..)
  , Font
  , displayTextLoc
  , parseTextLoc
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Font = Text

data Quote = Quote
  { author :: Text
  , source :: Text
  , location :: Maybe TextLoc -- pages/lines
  , quote :: [Text] -- lines
  } deriving (Generic, Show)

instance Eq Quote where
  Quote a1 s1 l1 _ == Quote a2 s2 l2 _ = a1 == a2 && s1 == s2 && l1 == l2

instance Ord Quote where
  Quote a1 s1 l1 _ `compare` Quote a2 s2 l2 _
    | a1 /= a2 = compare a1 a2
    | s1 /= s2 = compare s1 s2
    | l1 /= l2 = compare l1 l2
    | otherwise = EQ

data TextLoc
  = Line Natural
  | LineF Natural
  | LineFF Natural
  | LineRange Natural
              Natural
  | Page Natural
         TextLoc
  deriving (Show, Eq)

lineBounds :: TextLoc -> Maybe (Natural, Natural)
lineBounds loc =
  case loc of
    Line x -> Just (x, x)
    LineF x -> Just (x, x + 1)
    LineFF x -> Just (x, x + 2)
    LineRange x y -> Just (x, y)
    Page x _ -> Just (x, x)

instance Ord TextLoc where
  Page n l1 `compare` Page m l2
    | n == m = l1 `compare` l2
    | otherwise = n `compare` m
  loc1 `compare` loc2 = lineBounds loc1 `compare` lineBounds loc2

displayTextLoc :: TextLoc -> String
displayTextLoc loc =
  case loc of
    Line x -> show x
    LineF x -> show x ++ "f"
    LineFF x -> show x ++ "ff"
    LineRange x y -> show x ++ "-" ++ show y
    Page n x -> show n ++ ":" ++ displayTextLoc x

parseTextLoc ::
     (Stream s, Token s ~ Char, Tokens s ~ Text) => Parsec () s TextLoc
parseTextLoc =
  try (Page <$> (nat <* char ':') <*> parseTextLoc) <|>
  try (LineRange <$> (nat <* char '-') <*> nat) <|>
  try (LineFF <$> (nat <* string "ff")) <|>
  try (LineF <$> (nat <* char 'f')) <|>
  (Line <$> (nat <* eof))
  where
    nat = fromInteger <$> decimal
