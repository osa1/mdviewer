{-# LANGUAGE OverloadedStrings #-}

module HtmlBuilder (renderContents) where

import Prelude hiding (putStrLn, readFile, writeFile, head)

import Control.Exception
import System.FilePath (takeExtension)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes (charset, type_)
import Text.Blaze.Html.Renderer.String

import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.RST
import Text.Pandoc.UTF8
import Text.Pandoc.Writers.HTML

type Reader = String -> Either PandocError Pandoc

renderContents :: FilePath -> Maybe FilePath -> IO (Maybe String)
renderContents input style = handle returnNothing $ do
    let ext    = takeExtension input
        reader
          | ext == ".rst" = readRST def
          | otherwise     = readMarkdown def
    html <- case style of
        Nothing  -> toPureHtmlString input reader
        Just css -> toStylishedHtmlString css input reader
    return (Just html)
  where
    returnNothing :: SomeException -> IO (Maybe String)
    returnNothing e = do
        putStrLn $ "renderContents: exception raised\n" ++ displayException e
        return Nothing

toPureHtmlString :: FilePath -> Reader -> IO String
toPureHtmlString input reader = pureHtmlString <$> readFile input <*> pure reader

pureHtmlString :: String -> Reader -> String
pureHtmlString contents reader = either throw buildHtml (reader contents)
    where buildHtml pandoc = renderHtml $ do
            docTypeHtml $ do
                head (meta ! charset "UTF-8")
                body (writeHtml def pandoc)


toStylishedHtmlString :: FilePath -> FilePath -> Reader -> IO String
toStylishedHtmlString css input reader = stylishedHtmlString <$> readFile css <*> readFile input <*> pure reader

stylishedHtmlString :: String -> String -> Reader -> String
stylishedHtmlString css contents reader = either throw (buildHtml css) (reader contents)
    where buildHtml css pandoc = renderHtml $ do
            docTypeHtml $ do
                head $ do
                    meta  ! charset "UTF-8"
                    style ! type_ "text/css"
                          $ string css
            body (writeHtml def pandoc)

