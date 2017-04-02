module Command (module Command, runApp) where

import System.Console.ArgParser
import System.Console.ArgParser.QuickParams

import Types

-- | Required to read Maybe parameters
instance RawRead a => RawRead (Maybe a) where
    rawParse s = do 
        (val, rem) <- rawParse s 
        return (Just val, rem)


-- | Main parsing function
parseCommand :: IO (CmdLnInterface Command)
parseCommand = setVersion "1.0.0"
           <$> setDescr "A basic Markdown viewer/converter with Css support"
           <$> setEpilog "Project page: https://github.com/agustinmista/mdviewer"             
           <$> parseSubcommand


-- | Compose main parser using subcommand parsers
parseSubcommand :: IO (CmdLnInterface Command)
parseSubcommand = mkSubParser 
    [ ("show", mkDefaultApp showParser "show") 
    , ("convert", mkDefaultApp convertParser "convert") 
    , ("list", mkDefaultApp (pure List) "list") ] 


-- | Show subcommand parser
showParser :: ParserSpec Command
showParser = Show
    `parsedBy` optPos  ""      "input"  `Descr` "markdown input file"
    `andBy`    optFlag Nothing "style"  `Descr` "css style to embed (either a build-in style or a .css path)"

-- | Convert subcommand parser
convertParser :: ParserSpec Command 
convertParser = Convert
    `parsedBy` reqPos          "input"  `Descr` "markdown input file"
    `andBy`    optFlag Nothing "output" `Descr` "output Html file"
    `andBy`    optFlag Nothing "style"  `Descr` "css style to embed"
