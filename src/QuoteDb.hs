{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module QuoteDb where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Text.Read (decimal)
import GHC.Generics (Generic)

bsStr = T.unpack . T.decodeUtf8

strBs = T.encodeUtf8 . T.pack

data Quote = Quote
  { author :: T.Text
  , source :: T.Text
  , location :: Maybe TextLoc -- pages/lines
  , quote :: [T.Text] -- lines
  } deriving (Generic, Show)

textLocToString :: TextLoc -> String
textLocToString loc =
  case loc of
    Line x -> show x
    LineF x -> show x ++ "f"
    LineFF x -> show x ++ "ff"
    LineRange x y -> show x ++ "-" ++ show y

instance FromRecord Quote where
  parseRecord v
    | length v == 4 =
      Quote <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> (T.splitOn " / " <$> (v .! 3))
    | otherwise = mzero

instance ToRecord Quote where
  toRecord (Quote a s l q) =
    record [toField a, toField s, toField l, toField (T.intercalate " / " q)]

instance Pretty TextLoc where
  pretty = pretty . textLocToString

instance Pretty Quote where
  pretty (Quote a s l q) =
    guil (vcat (map pretty q)) <> hardline <>
    indent 2 (parens $ pretty a <> colon <+> pretty s <> loc)
    where
      guil = enclose "»" "«"
      loc =
        if isNothing l
          then mempty
          else comma <+> pretty l

data TextLoc
  = Line Int
  | LineF Int
  | LineFF Int
  | LineRange Int
              Int
  deriving (Show)

instance FromField TextLoc where
  parseField f =
    case decimal $ T.decodeUtf8 f of
      Left err -> mzero
      Right (num, rest) ->
        case rest of
          "f" -> return $ LineF num
          "ff" -> return $ LineFF num
          "" -> return $ Line num
          _
            | Just ('-', endt) <- T.uncons rest
            , Right (end, _) <- decimal endt -> return $ LineRange num end
            | otherwise -> mzero

instance ToField TextLoc where
  toField = strBs . textLocToString

fileToQuotes :: FilePath -> IO [Quote]
fileToQuotes f = do
  ftext <- (BL.fromStrict . T.encodeUtf8) <$> T.readFile f
  case decode NoHeader ftext of
    Left err -> error err
    Right v -> return $ toList v

prettyQuotes :: [Quote] -> IO ()
prettyQuotes = putDoc . vcat . intersperse line . map pretty
