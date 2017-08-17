{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import QuoteDb

import Data.List (nub, sort)
import Data.Monoid ((<>))
import Options.Applicative
import qualified Data.Text as Text (Text, intercalate, isInfixOf, pack)
import qualified Data.Text.IO as Text (putStrLn)

data QuoteOutputType
    = Plain
    | LaTeX
    deriving (Show)

data QuoteComponent
    = Quotes
    | Sources
    | Authors
     deriving (Show, Eq)

data QuoteDbOptions = QuoteDbOptions
    { _dbFile :: FilePath
    , _outputType :: QuoteOutputType
    , _searchDomain :: Maybe [QuoteComponent]
    , _searchPattern :: Maybe Text.Text
    } deriving (Show)

quoteDbOptions :: Parser QuoteDbOptions
quoteDbOptions =
    QuoteDbOptions <$> quoteDbFile <*> quoteOutputType <*> quoteSearchDomain <*>
    quoteSearchPattern
  where
    quoteDbFile =
        strOption
            (long "db-file" <> short 'f' <> metavar "CSV" <>
             help "CSV quote database")
    quoteOutputType =
        flag' Plain (long "plain" <> help "Output in plain text (default)") <|>
        flag Plain LaTeX (long "latex" <> help "Output in LaTeX")
    quoteSearchDomain =
        optional . fmap nub . some $
        flag' Quotes (short 'q' <> long "quotes" <> help "Search quotes") <|>
        flag' Sources (short 's' <> long "sources" <> help "Search sources") <|>
        flag' Authors (short 'a' <> long "authors" <> help "Search authors")
    quoteSearchPattern =
        optional $
        Text.pack <$> strArgument (metavar "PATTERN" <> help "Search string")

main :: IO ()
main = do
    QuoteDbOptions df ot sd sp <- execParser quoteDbOptions'
    quotes <- fileToQuotes df
    let matching =
            case sp of
                Nothing -> quotes
                Just pat -> searchQuotes sd pat quotes
    Text.putStrLn $
        flip id (sort matching) $
        case ot of
            Plain -> renderWith prettyQuote
            LaTeX -> mkStandalone . renderWith laTeXQuote
  where
    quoteDbOptions' = info (quoteDbOptions <**> helper) fullDesc
    renderWith r = Text.intercalate "\n\n" . map r
    searchQuotes dom pat =
        filter $
        \q -> any (Text.isInfixOf pat) $ concatMap (select q) $ sequence dom
    select (Quote a s _ q) =
        maybe ([a, s] ++ q) $
        \dom ->
             case dom of
                 Quotes -> q
                 Sources -> [s]
                 Authors -> [a]

