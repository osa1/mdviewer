{-# LANGUAGE OverloadedStrings #-}

module HtmlBuilder (renderContents) where

import Prelude hiding (putStrLn, readFile, writeFile, head)

import Control.Exception

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes (charset, type_)
import Text.Blaze.Html.Renderer.String

import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Options
import Text.Pandoc.UTF8

renderContents :: FilePath -> Maybe FilePath -> IO (Maybe String)
renderContents input style = handle returnNothing $ do
    html <- case style of
        Nothing  -> toPureHtmlString input
        Just css -> toStylishedHtmlString css input
    return (Just html)
  where
    returnNothing :: SomeException -> IO (Maybe String)
    returnNothing e = do
        putStrLn $ "renderContents: exception raised\n" ++ displayException e
        return Nothing

toPureHtmlString :: FilePath -> IO String
toPureHtmlString input = pureHtmlString <$> readFile input

pureHtmlString :: String -> String
pureHtmlString contents = either throw buildHtml (readMarkdown def contents)
    where buildHtml pandoc = renderHtml $ do
            docTypeHtml $ do
                head (meta ! charset "UTF-8") 
                body (writeHtml def pandoc)


toStylishedHtmlString :: FilePath -> FilePath -> IO String
toStylishedHtmlString css input = stylishedHtmlString <$> readFile css <*> readFile input
    
stylishedHtmlString :: String -> String -> String
stylishedHtmlString css contents = either throw (buildHtml css) (readMarkdown def contents)
    where buildHtml css pandoc = renderHtml $ do
            docTypeHtml $ do
                head $ do
                    meta  ! charset "UTF-8" 
                    style ! type_ "text/css"
                          $ string css
            body (writeHtml def pandoc)

