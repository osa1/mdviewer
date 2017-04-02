module Convert (runConvert) where

import Prelude hiding (writeFile)

import Text.Pandoc.UTF8

import System.FilePath
import System.Exit

import Types
import HtmlBuilder

runConvert :: Maybe Style -> FilePath -> Maybe FilePath -> IO ()
runConvert mb_style input mb_output = do
    mb_style_path <- mapM stylePath mb_style
    result <- renderContents input mb_style_path
    case result of
        Nothing   -> die "ABORTING!"
        Just html -> do
            writeFile output html
                where output | Just output <- mb_output = output
                             | otherwise                = input -<.> "html"
