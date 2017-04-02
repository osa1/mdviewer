{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Show (runShow) where

import Prelude hiding (writeFile)

import Data.Maybe
import Data.List (elemIndex)

import System.FilePath
import System.Exit

import Control.Conditional
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.IORef

import Graphics.UI.Gtk hiding (Style)
import Graphics.UI.Gtk.WebKit.WebView

import Text.Pandoc.UTF8 (writeFile)

import Types
import HtmlBuilder


offsetStyleFrom :: Int -> Styles -> Maybe Style -> Style
offsetStyleFrom offset ss mb_s =
    style_list !! (s_idx `mod` length style_list)
  where
    style_list = styleList ss
    s_idx
      | Just s <- mb_s
      = fromJust (elemIndex s style_list) + offset
      | otherwise
      = offset

nextStyleFrom :: Styles -> Maybe Style -> Style
nextStyleFrom = offsetStyleFrom 1

prevStyleFrom :: Styles -> Maybe Style -> Style
prevStyleFrom = offsetStyleFrom (-1)


setContent :: WebView -> String -> IO ()
setContent webview html = webViewLoadString webview html Nothing ""


makeTitle :: String -> Maybe FilePath -> String
makeTitle input mb_style = status ++ "  -  Markdown Viewer"
    where status | Just style <- mb_style = input ++ "@" ++ style
                 | otherwise              = input


genericDialogNew :: String -> Window -> IO FileChooserDialog
genericDialogNew action window = fileChooserDialogNew
    (Just action) (Just window)
    FileChooserActionSave
    [ (action, ResponseAccept)
    , ("Cancel", ResponseCancel) ]


saveDialogNew, openDialogNew :: Window -> IO FileChooserDialog
saveDialogNew = genericDialogNew "Save"
openDialogNew = genericDialogNew "Open"


whenReturnFilename :: FileChooserDialog -> (FilePath -> IO ()) -> IO ()
whenReturnFilename dialog action = do
    response <- dialogRun dialog
    case response of
        ResponseAccept -> do
            dialogVal <- fileChooserGetFilename dialog
            case dialogVal of
                Just path -> action path
                _ -> return ()
        _ -> return ()


abortDialog :: Window ->  IO ()
abortDialog window = do
    dialog <- messageDialogNew (Just window) [] MessageError ButtonsOk
                ("An internal error happened. Aborting" :: String)
    _ <- dialogRun dialog
    widgetDestroy dialog
    exitFailure

invalidFileDialog :: Window -> String -> IO ()
invalidFileDialog window path = do
    dialog <- messageDialogNew (Just window) [] MessageError ButtonsOk
                ("Unable to load " ++ path)
    _ <- dialogRun dialog
    widgetDestroy dialog

runShow :: Styles -> Maybe Style -> FilePath -> IO ()
runShow styles style0 input0 = do
    -- Create an "global" state that keeps the style and the current file
    -- displayed between different events handles
    style <- newIORef style0
    input <- newIORef input0
    fullscreen <- newIORef False
    lastPos <- newIORef Nothing

    -- Initialize the GUI
    void initGUI

    -- Create the widgets
    window <- windowNew
    scrolled <- scrolledWindowNew Nothing Nothing
    webview <- webViewNew

    -- Set widgets default attributes
    window `set` [ windowTitle          := makeTitle input0 (fmap styleName style0)
                 , windowResizable      := True
                 , windowWindowPosition := WinPosCenter
                 , windowDefaultWidth   := 640
                 , windowDefaultHeight  := 640
                 , containerChild       := scrolled ]

    scrolled `set` [ containerChild := webview ]

    do
      style0_path <- mapM stylePath style0
      result <- renderContents input0 style0_path
      maybe (invalidFileDialog window input0 >> exitFailure)
            (setContent webview) result

    -- Handle events
    _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "F11" <- eventKeyName
        liftIO $ do
            isFullscreen <- readIORef fullscreen
            writeIORef fullscreen (not isFullscreen)
            if isFullscreen
                then windowUnfullscreen window
                else windowFullscreen window

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "q" <- eventKeyName
        liftIO $ mainQuit >> exitSuccess

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "r" <- eventKeyName
        liftIO $ do

            uri <- webViewGetUri webview
            let isInputFile = maybe False (==[]) uri

            when isInputFile $ do
                adj <- scrolledWindowGetVAdjustment scrolled
                pos <- adjustmentGetValue adj
                writeIORef lastPos (Just pos)


            style' <- readIORef style >>= mapM stylePath
            input' <- readIORef input
            result <- renderContents input' style'
            maybe (abortDialog window) (setContent webview) result

    _ <- webview `after` loadFinished $ \_ ->  do
        liftIO $ do
            pos <- readIORef lastPos
            when (isJust pos) $ do
                adj <- scrolledWindowGetVAdjustment scrolled
                adjustmentSetValue adj (fromJust pos)
                adjustmentValueChanged adj

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "j" <- eventKeyName
        liftIO $ do
            adj <- scrolledWindowGetVAdjustment scrolled
            ps <- adjustmentGetStepIncrement adj
            pos <- adjustmentGetValue adj
            adjustmentSetValue adj (pos + ps)
            adjustmentValueChanged adj

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "k" <- eventKeyName
        liftIO $ do
            adj <- scrolledWindowGetVAdjustment scrolled
            ps <- adjustmentGetStepIncrement adj
            pos <- adjustmentGetValue adj
            adjustmentSetValue adj (pos - ps)
            adjustmentValueChanged adj

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "g" <- eventKeyName
        liftIO $ do
            adj <- scrolledWindowGetVAdjustment scrolled
            top <- adjustmentGetLower adj
            adjustmentSetValue adj top
            adjustmentValueChanged adj

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "G" <- eventKeyName
        liftIO $ do
            adj <- scrolledWindowGetVAdjustment scrolled
            bottom <- adjustmentGetUpper adj
            adjustmentSetValue adj bottom
            adjustmentValueChanged adj

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "z" <- eventKeyName
        liftIO $ do
            canGoBack <- webViewCanGoBack webview
            if canGoBack
                then webViewGoBack webview
                else do
                    input' <- readIORef input
                    style' <- readIORef style >>= mapM stylePath
                    result <- renderContents input' style'
                    maybe (abortDialog window) (setContent webview) result

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "x" <- eventKeyName
        liftIO $ do
            canGoForward <- webViewCanGoForward webview
            when (canGoForward) (webViewGoForward webview)

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "n" <- eventKeyName
        liftIO $ do
            modifyIORef style (Just . nextStyleFrom styles)
            style' <- readIORef style
            input' <- readIORef input
            result <- mapM stylePath style' >>= renderContents input'
            maybe (abortDialog window) (setContent webview) result
            window `set` [ windowTitle := makeTitle input' (styleName <$> style') ]

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "N" <- eventKeyName
        liftIO $ do
            modifyIORef style (Just . prevStyleFrom styles)
            style' <- readIORef style
            input' <- readIORef input
            result <- mapM stylePath style' >>= renderContents input'
            maybe (abortDialog window) (setContent webview) result
            window `set` [ windowTitle := makeTitle input' (styleName <$> style') ]

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "e" <- eventKeyName
        liftIO $ do

            dialog <- openDialogNew window
            filter <- fileFilterNew
            fileFilterAddMimeType filter ("text/plain" :: String)
            fileChooserAddFilter dialog filter
            widgetShow dialog
            dialog `whenReturnFilename` \path -> do
                putStrLn $ "Opening file from " ++ path
                writeIORef input path
                style' <- readIORef style
                result <- mapM stylePath style' >>= renderContents path
                case result of
                    Nothing -> invalidFileDialog window path
                    Just html' -> do
                        webview `setContent` html'
                        window `set` [ windowTitle := makeTitle path (styleName <$> style') ]

            widgetDestroy dialog

    _ <- window `on` keyPressEvent $ tryEvent $ do
        "w" <- eventKeyName
        liftIO $ do

            dialog <- saveDialogNew window
            widgetShow dialog
            dialog `whenReturnFilename` \path -> do

                input' <- readIORef input
                style' <- readIORef style
                result <- mapM stylePath style' >>= renderContents input'
                maybe (abortDialog window) (\html' -> do

                    let path' = if hasExtension path
                                then path
                                else path <.> "html"

                    putStrLn $ "Saving html file to " ++ path'
                    writeFile path' html'
                    ) result

            widgetDestroy dialog

    -- Start the GUI main loop
    widgetShowAll window
    mainGUI
