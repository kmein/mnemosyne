{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module QuoteDb.Html
  ( mkHtmlDocument
  ) where


import QuoteDb.Type hiding (quote)
import QuoteDb.Util (groupOn, intersperseM_)

import Control.Monad (forM_)
import Data.Maybe (listToMaybe)
import Hasmin (minifyCSS)
import Lucid
import qualified Data.Text as Text (Text, pack, splitOn)
import qualified Data.Text.Lazy as Text (toStrict)
import Text.RawString.QQ

instance ToHtml Quote where
  toHtmlRaw = toHtml
  toHtml (Quote a s l q) =
    div_ [class_ "quote"] $ do
      blockquote_ $ p_ $ intersperseM_ (br_ []) quoteH
      cite_ $ do
        span_ [class_ "author"] authorH
        span_ [class_ "source"] sourceH
        case l of
          Just loc -> span_ [class_ "location"] $ toHtml loc
          Nothing -> mempty
    where
      quoteH = map toHtml q
      authorH = toHtml $ head $ Text.splitOn "/" a
      sourceH = toHtml s

instance ToHtml TextLoc where
  toHtmlRaw = toHtml
  toHtml = toHtml . toText
    where
      toText loc =
        case loc of
          Line x -> show x
          LineF x -> show x ++ "f."
          LineFF x -> show x ++ "ff."
          LineRange x y -> show x ++ "–" ++ show y
          Page x l -> show x ++ "," ++ toText l

mkHtmlDocument :: Maybe FilePath -> [Quote] -> Text.Text
mkHtmlDocument css = Text.toStrict . renderText . mkHtmlDocument' css

mkHtmlDocument' :: Maybe FilePath -> [Quote] -> Html ()
mkHtmlDocument' css qs = do
  doctype_
  html_ [lang_ "de"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      case css of
        Nothing -> style_ defaultCss
        Just path -> link_ [rel_ "stylesheet", href_ (Text.pack path)]
    body_ $
      forM_ (groupOn author qs) $ \authorQs ->
        section_ $ do
          mkHeader (author <$> listToMaybe authorQs)
          forM_ (groupOn source authorQs) $ \sourceQs ->
            article_ $ mapM_ toHtml sourceQs
  where
    mkHeader :: Maybe Text.Text -> Html ()
    mkHeader auth =
      case Text.splitOn "/" <$> auth of
        Just [a] -> h1_ [class_ "author-head"] $ toHtml a
        Just [a, t] ->
          h1_ $ do
            span_ [class_ "author-head"] $ toHtml a
            span_ [class_ "translator-head"] $ toHtml t
        _ -> return ()

defaultCss :: Text.Text
defaultCss = either error id $ minifyCSS [r|
  .author, .location { font-style: normal }
  body {
    margin:auto;
    max-width: 700px;
    line-height: 1.5;
    background-color: #444
  }
  .author::before {
    content: "— ";
    color: grey
  }
  .author::after { content: ":  " }
  .location::before { content: ", " }
  article+article::before {
    content: "* * *";
    color: grey;
    display: block;
    font-size: 1.5em;
    text-align: center;
    padding-top: 3%
  }
  section {
    color: #444;
    background-color: #fff;
    text-align: justify;
    text-justify: inter-word;
    padding: 1% 5% 5%;
    margin: 5%;
    -webkit-box-shadow: 12px 12px 26px -9px rgba(0,0,0,.375);
    -moz-box-shadow: 12px 12px 26px -9px rgba(0,0,0,.375);
    box-shadow: 12px 12px 26px -9px rgba(0,0,0,.375)
  }
  section:hover {
    -webkit-box-shadow: 12px 12px 26px -9px rgba(0,0,0,.75);
    -moz-box-shadow: 12px 12px 26px -9px rgba(0,0,0,.75);
    box-shadow: 12px 12px 26px -9px rgba(0,0,0,.75)
  }
  h1 { text-align:center }
  h1::after, h1::before {
    content: "—";
    padding: 0 8px;
    color: grey
  }
  h1 .author-head { display: none }
  h1 .translator-head::before {
    content: "transl. ";
    font-style: italic;
    color: grey
  }
|]
