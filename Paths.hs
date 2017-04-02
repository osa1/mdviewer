module Paths where

import System.Directory
import System.FilePath

progName, aboutFile, dataRelPath, stylesRelPath :: FilePath

progName = "mdviewer"

aboutFile = "about.md"

dataRelPath = "data"
stylesRelPath = "styles"

getStylesSourcePath :: IO FilePath
getStylesSourcePath = makeAbsolute $ dataRelPath </> stylesRelPath

getAboutSourcePath :: IO FilePath
getAboutSourcePath = makeAbsolute $ dataRelPath </> aboutFile

getDataPath :: IO FilePath
getDataPath = getXdgDirectory XdgData progName

getConfigPath :: IO FilePath
getConfigPath = getXdgDirectory XdgConfig progName

getCachePath :: IO FilePath
getCachePath = getXdgDirectory XdgCache progName

getStylesPath :: IO FilePath
getStylesPath = (</> stylesRelPath) <$> getDataPath

getAboutFile :: IO FilePath
getAboutFile = (</> aboutFile) <$> getDataPath
