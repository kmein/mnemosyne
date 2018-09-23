{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import QuoteDb

import Data.List (nub, sort, intersect)
import Data.Monoid ((<>))
import qualified Data.Text as Text
  ( Text
  , cons
  , intercalate
  , isInfixOf
  , pack
  , replace
  )
import qualified Data.Text.IO as Text (putStrLn)
import Options.Applicative

data QuoteOutputType
  = Plain
  | LaTeX
  | Html
  deriving (Show)

data Search
  = SearchQuotes { pattern :: Text.Text }
  | SearchSources { pattern :: Text.Text }
  | SearchAuthors { pattern :: Text.Text }
  deriving (Show, Eq)

data QuoteDbOptions = QuoteDbOptions
  { _dbFile :: FilePath
  , _outputType :: QuoteOutputType
  , _outputFont :: Maybe Font
  , _outputCss :: Maybe FilePath
  , _queries :: Maybe [Search]
  } deriving (Show)

quoteDbOptions :: Parser QuoteDbOptions
quoteDbOptions =
  QuoteDbOptions <$> quoteDbFile <*> quoteOutputType <*> quoteOutputFont <*>
  quoteOutputCss <*>
  quoteSearchDomain
  where
    quoteDbFile = strArgument (metavar "CSV" <> help "CSV quote database")
    quoteOutputType =
      flag' Plain (long "plain" <> help "Output in plain text (default)") <|>
      flag Plain Html (long "html" <> help "Output in HTML") <|>
      flag Plain LaTeX (long "latex" <> help "Output in LaTeX")
    quoteSearchDomain =
      optional . fmap nub . some $
      fmap
        (SearchQuotes . Text.pack)
        (strOption
           (short 'q' <> long "quotes" <> help "Search quotes" <> metavar "STR")) <|>
      fmap
        (SearchSources . Text.pack)
        (strOption
           (short 's' <> long "sources" <> help "Search sources" <>
            metavar "STR")) <|>
      fmap
        (SearchAuthors . Text.pack)
        (strOption
           (short 'a' <> long "authors" <> help "Search authors" <>
            metavar "STR"))
    quoteOutputCss =
      optional $
      strOption
        (long "css" <> metavar "PATH" <>
         help "The CSS to be used for HTML output")
    quoteOutputFont =
      optional $
      Text.pack <$>
      strOption
        (long "font" <> metavar "FONTNAME" <>
         help "The font to be used for LaTeX output")

main :: IO ()
main = do
  QuoteDbOptions csv ot font css queries <- execParser quoteDbOptions'
  quotes <- fileToQuotes csv
  let matching = maybe quotes (`searchQuotes` quotes) queries
  Text.putStrLn $
    flip id (sort matching) $
    case ot of
      Html -> mkHtmlDocument css
      Plain -> Text.intercalate "\n\n" . map prettyQuote
      LaTeX -> wrap "\\section" . mkLaTeXDocument font
  where
    wrap at = Text.replace at ('\n' `Text.cons` at)
    quoteDbOptions' = info (quoteDbOptions <**> helper) fullDesc
    searchQuotes ss qs =
      foldl1 intersect $
      map (\s -> filter (matches s) qs) ss
    matches search (Quote a s _ q) =
      case search of
        SearchQuotes pat -> any (Text.isInfixOf pat) q
        SearchSources pat -> Text.isInfixOf pat s
        SearchAuthors pat -> Text.isInfixOf pat a
