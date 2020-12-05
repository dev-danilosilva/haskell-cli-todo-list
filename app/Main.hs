module Main where

import Lib
import Options.Applicative


defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath
    <> long    "data-path"
    <> short   'p'
    <> metavar "DATAPATH"
    <> help    ("Path to the data file (default " ++ defaultDataPath ++ ")")

type ItemIndex = Int

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

main :: IO ()
main =do
    print greetings
    dataPath <- execParser (info dataPathParser (progDesc "To do List Manager"))
    print dataPath
    return ()
