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
  , _searches :: Maybe [Search]
  } deriving (Show)

quoteDbOptions :: Parser QuoteDbOptions
quoteDbOptions =
  QuoteDbOptions <$> quoteDbFile <*> quoteOutputType <*> quoteOutputFont <*>
  quoteSearchDomain
  where
    quoteDbFile = strArgument (metavar "CSV" <> help "CSV quote database")
    quoteOutputType =
      flag' Plain (long "plain" <> help "Output in plain text (default)") <|>
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
    quoteOutputFont =
      optional $
      Text.pack <$>
      strOption
        (short 'f' <> long "font" <> metavar "FONTNAME" <>
         help "The font to be used for LaTeX output")

main :: IO ()
main = do
  QuoteDbOptions df ot fo ss <- execParser quoteDbOptions'
  quotes <- fileToQuotes df
  let matching = maybe quotes (`searchQuotes` quotes) ss
  Text.putStrLn $
    flip id (sort matching) $
    case ot of
      Plain -> Text.intercalate "\n\n" . map prettyQuote
      LaTeX -> wrap "\\section" . mkLaTeXDocument fo
  where
    wrap at = Text.replace at ('\n' `Text.cons` at)
    quoteDbOptions' = info (quoteDbOptions <**> helper) fullDesc
    searchQuotes ss qs =
      foldl1 intersect $
      map (\s -> filter (any (Text.isInfixOf (pattern s)) . select s) qs) ss
    select search (Quote a s _ q) =
      case search of
        SearchQuotes {} -> q
        SearchSources {} -> [s]
        SearchAuthors {} -> [a]
