module Types where

import Control.Conditional (unlessM)
import Data.Maybe
import System.Directory
import System.FilePath

import Paths

newtype Style = Style { styleName :: String }
  deriving (Eq)

-- | Styles live in `stylesPath`. Use `stylePath` to get the full path.
newtype Styles = Styles { styleList :: [Style] }

addStyle :: Style -> Styles -> Styles
addStyle s (Styles ss) = Styles (s : ss)

-- | Convert a style name to full .css path.
stylePath :: Style -> IO FilePath
stylePath (Style style) = do
    styles_path <- getStylesPath
    return (styles_path </> style <.> "css")

loadStyles :: IO Styles
loadStyles = do
    stylesPath <- getStylesPath
    unlessM (doesPathExist stylesPath)
            (error ("error loading style files from " ++ show stylesPath))
    files <- listDirectory stylesPath
    let styles = filter ((== ".css") . takeExtension) files
    return (Styles (map (Style . takeBaseName) styles))

-- | Supported commands and parameters
data Command
    = Show
        { input :: FilePath
        , style :: Maybe String }
    | Convert
        { input  :: FilePath
        , output :: Maybe FilePath
        , style  :: Maybe String }
    | List
    deriving (Show)

usesOutput :: Command -> Bool
usesOutput = isJust . output

getOutput :: Command -> String
getOutput = fromJust . output
