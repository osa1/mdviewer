module Main (main) where

import Control.Exception
import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Process (forkProcess)

import Types
import Command
import Show
import Convert
import List
import Paths

dispatcher :: Styles -> Command -> IO ()
dispatcher styles List = runList styles
dispatcher styles (Show "" _) = do
    about <- getAboutFile
    void $ forkProcess $ runShow styles (Just (Style "markdown")) about
dispatcher styles (Show input mb_style) = do
    (mb_style', styles') <- sanitizeStyle mb_style styles
    void $ forkProcess $ runShow styles' mb_style' input
dispatcher styles (Convert input mb_output mb_style) = do
    (mb_style', _) <- sanitizeStyle mb_style styles
    runConvert mb_style' input mb_output

sanitizeStyle
    :: Maybe String
         -- ^ A file path to a .css to be used as style, or a built-in style
         -- name.
    -> Styles
         -- ^ Style index
    -> IO (Maybe Style, Styles)
sanitizeStyle Nothing   sts
  = return (Nothing, sts)
sanitizeStyle (Just st) sts
  | Style st `elem` styleList sts
  = do putStrLn $ "Using built-in style: " ++ st
       return (Just (Style st), sts)
  | otherwise
  = do fileExists <- doesFileExist st
       let isCSS = takeExtension st == ".css"
       if fileExists && isCSS
         then do
           stylesPath <- getStylesPath
           let stName   = takeBaseName st
               copyPath = stylesPath </> stName <.> "css"
           putStrLn $ "Using external style from "++ st ++" as " ++ stName
           copyFile st copyPath
           putStrLn $ "External style copied to " ++ copyPath
           return (Just (Style st), addStyle (Style st) sts)
         else do
           putStrLn $ "Provided style is not built-in nor a valid CSS\
                       \ file. Using pure HTML instead"
           return (Nothing, sts)

main :: IO ()
main = handle printException $ do
    command <- parseCommand
    styles <- loadStyles
    runApp command (dispatcher styles)
  where
    printException :: SomeException -> IO ()
    printException e = do
        exe <- getProgName
        putStrLn $ exe ++ ": " ++ show e
