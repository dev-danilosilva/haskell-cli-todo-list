module Main where

import Options.Applicative hiding (infoParser)

type ItemIndex = Int

type ItemDescription = Maybe String

data Options = Options FilePath Command deriving (Show)

data Command
    = Info
    | Init
    | Add
    | View
    | List
    | Update
    | Remove
    | PathCheck
    deriving Show

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

addParser :: Parser Command
addParser = pure Add

viewParser :: Parser Command
viewParser = pure View

updateParser :: Parser Command
updateParser = pure Update

removeParser :: Parser Command
removeParser = pure Remove

listParser :: Parser Command
listParser = pure List

pathCheckParser :: Parser Command
pathCheckParser = pure PathCheck

commandParser :: Parser Command
commandParser = subparser $
       command "info"    (info infoParser   (progDesc "Show Info"))
    <> command "init"    (info initParser   (progDesc "Initialize a new to do List"))
    <> command "add"     (info addParser    (progDesc "Add a new item to the list"))
    <> command "view"    (info viewParser   (progDesc "View item"))
    <> command "list"    (info listParser   (progDesc "List all list items"))
    <> command "update"  (info updateParser (progDesc "Update a list item"))
    <> command "remove"  (info removeParser (progDesc "Remove a list item"))
    <> command "path"    (info pathCheckParser (progDesc "Shows Current Data Path"))

optionsParser :: Parser Options
optionsParser= Options
    <$> dataPathParser
    <*> commandParser

dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath
    <> long    "data-path"
    <> short   'p'
    <> metavar "DATAPATH"
    <> help    ("Path to the data file (default " ++ defaultDataPath ++ ")")

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "Index of an item")

itemDescriptionValueParser :: Parser String
itemDescriptionValueParser = strOption $
       long     "desc"
    <> short    'd'
    <> metavar  "ITEMDESCRIPTION"
    <> help     "Item Description"

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
        Just <$> itemDescriptionValueParser
    <|> flag' Nothing (long "clear-desc")


run :: FilePath -> Command -> IO ()
run filePath comm = case comm of
    Info      -> print "Loading Info..."
    Init      -> print "Initializing..."
    Add       -> print "Adding To Do..."
    List      -> print "Listing Items..."
    Remove    -> print "Removing Item..."
    Update    -> print "Updating Item..."
    View      -> print "Loading View..."
    PathCheck -> print filePath

main :: IO ()
main =do
    Options dataPath comm <- execParser (info optionsParser (progDesc "To do List Manager"))
    run dataPath comm
