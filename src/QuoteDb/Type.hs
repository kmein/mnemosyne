{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module QuoteDb.Type
  ( Quote(..)
  , TextLoc(..)
  , displayTextLoc
  , parseTextLoc
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Megaparsec
import Text.Megaparsec.Lexer
import Numeric.Natural

data Quote = Quote
    { author :: Text
    , source :: Text
    , location :: Maybe TextLoc -- pages/lines
    , quote :: [Text] -- lines
    } deriving (Generic, Show)

instance Eq Quote where
    Quote a1 s1 l1 _ == Quote a2 s2 l2 _ = a1 == a2 && s1 == s2 && l1 == l2

instance Ord Quote where
    Quote a1 s1 l1 _ <= Quote a2 s2 l2 _
        | a1 == a2 = s1 <= s2
        | s1 == s2 = l1 <= l2
        | otherwise = a1 <= a2

data TextLoc
    = Line Natural
    | LineF Natural
    | LineFF Natural
    | LineRange Natural
                Natural
    | Page Natural
           TextLoc
     deriving (Show, Eq)

instance Ord TextLoc where
    loc1 <= loc2 = firstLine loc1 <= firstLine loc2
      where
        firstLine l =
            case l of
                Line x -> Just x
                LineF x -> Just x
                LineFF x -> Just x
                LineRange x _ -> Just x
                Page _ _ -> Nothing

displayTextLoc :: TextLoc -> String
displayTextLoc loc =
    case loc of
        Line x -> show x
        LineF x -> show x ++ "f"
        LineFF x -> show x ++ "ff"
        LineRange x y -> show x ++ "-" ++ show y
        Page n x -> show n ++ ":" ++ displayTextLoc x

parseTextLoc
    :: (Stream s, Token s ~ Char)
    => Parsec () s TextLoc
parseTextLoc =
    try (Page <$> (nat <* char ':') <*> parseTextLoc) <|>
    try (LineRange <$> (nat <* char '-') <*> nat) <|>
    try (LineFF <$> (nat <* string "ff")) <|>
    try (LineF <$> (nat <* char 'f')) <|>
    (Line <$> (nat <* eof))
  where
    nat = fromInteger <$> integer
