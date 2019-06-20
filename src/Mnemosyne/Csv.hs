{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Mnemosyne.Csv
  ( fileToQuotes
  ) where

import Mnemosyne.Type

import Control.Monad (mzero)
import Data.ByteString.Lazy (fromStrict)
import Data.Csv
import Data.Foldable (toList)
import qualified Data.Text as Text (intercalate, pack, replace, splitOn)
import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as Text (readFile)
import Text.Megaparsec (parse)

instance FromField TextLoc where
  parseField =
    either (fail . show) return . parse parseTextLoc "input" . Text.decodeUtf8

instance ToField TextLoc where
  toField = Text.encodeUtf8 . Text.pack . displayTextLoc

instance FromRecord Quote where
  parseRecord v
    | length v == 4 =
      Quote <$> v .! 0 <*> v .! 1 <*> v .! 2 <*>
      ((Text.splitOn " | " . Text.replace " || " " |  | ") <$> (v .! 3))
    | otherwise = mzero

instance ToRecord Quote where
  toRecord (Quote a s l q) =
    record
      [ toField a
      , toField s
      , toField l
      , toField $ Text.replace " |  | " " || " $ Text.intercalate " | " q
      ]

fileToQuotes :: FilePath -> IO [Quote]
fileToQuotes f = do
  ftext <- (fromStrict . Text.encodeUtf8) <$> Text.readFile f
  either fail (return . toList) $ decode NoHeader ftext
