module Main where

import QuoteDb

main :: IO ()
main = prettyQuotes =<< fileToQuotes "sample.csv"
