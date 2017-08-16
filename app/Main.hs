{-# LANGUAGE OverloadedStrings #-}
module Main where

import QuoteDb

import Data.Monoid ((<>))
import Options.Applicative
import qualified Data.Text as Text (intercalate)
import qualified Data.Text.IO as Text (putStrLn)

data QuoteOutputType
    = Plain
    | LaTeX
     deriving (Show)

data QuoteDbOptions = QuoteDbOptions
    { dbFile :: FilePath
    -- action: search, add, export
    , outputType :: QuoteOutputType
    } deriving (Show)

quoteDbOptions :: Parser QuoteDbOptions
quoteDbOptions = QuoteDbOptions <$> quoteDbFile <*> quoteOutputType
  where
    quoteDbFile =
        strOption
            (long "db-file" <> short 'f' <> metavar "CSV" <>
             help "CSV quote database")
    quoteOutputType =
        flag' Plain (long "plain" <> help "Output in plain text (default)") <|>
        flag Plain LaTeX (long "latex" <> help "Output in LaTeX")

main :: IO ()
main = do
    QuoteDbOptions f ot <- execParser quoteDbOptions'
    quotes <- fileToQuotes f
    case ot of
      Plain -> renderWith prettyQuote quotes
      LaTeX -> renderWith laTeXQuote quotes
  where
    quoteDbOptions' = info (quoteDbOptions <**> helper) fullDesc
    renderWith r = Text.putStrLn . Text.intercalate "\n\n" . map r
