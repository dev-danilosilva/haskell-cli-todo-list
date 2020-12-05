module Main where

import Options.Applicative hiding (infoParser)


data Options = Options FilePath Command deriving Show

type ItemIndex       = Int
type ItemTitle       = String
type ItemDescription = Maybe String
type ItemPriority    = Maybe String
type ItemDueBy       = Maybe String

data ItemUpdate = ItemUpdate
    { titleUpdate       :: Maybe ItemTitle
    , descriptionUpdate :: Maybe ItemDescription
    , priorityUpdate    :: Maybe ItemPriority
    , dueByUpdate       :: Maybe ItemDueBy
    } deriving Show

data Item = Item
    { title       :: ItemTitle
    , description :: ItemDescription
    , priority    :: ItemPriority
    , dueBy       :: ItemDueBy
    } deriving Show


data Command
    = Info
    | Init
    | Add        Item
    | View       ItemIndex
    | List
    | Update     ItemIndex ItemUpdate
    | Remove     ItemIndex
    | PathCheck
    deriving Show

defaultDataPath :: FilePath
defaultDataPath = "~/.to-do.yaml"

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

addParser :: Parser Command
addParser = Add <$> addItemParser

addItemParser :: Parser Item
addItemParser = Item
    <$> argument str (metavar "TITLE" <> help "Item Title")
    <*> optional itemDescriptionValueParser
    <*> optional itemPriorityValueParser
    <*> optional itemDueByValueParser

viewParser :: Parser Command
viewParser = View <$> itemIndexParser

removeParser :: Parser Command
removeParser = Remove <$> itemIndexParser

listParser :: Parser Command
listParser = pure List

pathCheckParser :: Parser Command
pathCheckParser = pure PathCheck


updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

updateItemParser :: Parser ItemUpdate
updateItemParser = ItemUpdate
    <$> optional updateItemTitleParser
    <*> optional updateItemDescriptionParser
    <*> optional updateItemPriorityParser
    <*> optional updateItemDueByParser

commandParser :: Parser Command
commandParser = subparser $
       command "info"    (info infoParser      (progDesc "Show Info"))
    <> command "init"    (info initParser      (progDesc "Initialize a new to do List"))
    <> command "add"     (info addParser       (progDesc "Add a new item to the list"))
    <> command "view"    (info viewParser      (progDesc "View item"))
    <> command "list"    (info listParser      (progDesc "List all list items"))
    <> command "update"  (info updateParser    (progDesc "Update a list item"))
    <> command "remove"  (info removeParser    (progDesc "Remove a list item"))
    <> command "path"    (info pathCheckParser (progDesc "Shows Current Data Path"))

optionsParser :: Parser Options
optionsParser= Options
    <$> dataPathParser
    <*> commandParser

dataPathParser :: Parser FilePath
dataPathParser = strOption $
    value defaultDataPath
    <> long    "data-path"
    <> short   'P'
    <> metavar "DATAPATH"
    <> help    ("Path to the data file (default " ++ defaultDataPath ++ ")")


itemDescriptionValueParser :: Parser String
itemDescriptionValueParser = strOption $
       long     "desc"
    <> short    'd'
    <> metavar  "DESCRIPTION"
    <> help     "Item Description"

itemTitleValueParser :: Parser String
itemTitleValueParser = strOption $
        long    "title"
    <>  short   't'
    <>  metavar "TITLE"
    <>  help    "Item Title"

itemDueByValueParser :: Parser String
itemDueByValueParser = strOption $
        long    "due-by"
    <>  short   'b'
    <>  metavar "DUEBY"
    <>  help    "Due-By"

itemPriorityValueParser :: Parser String
itemPriorityValueParser = strOption $
        long    "priority"
    <>  short   'p'
    <>  metavar "PRIORITY"
    <>  help    "Item Priority"

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "Index of an item")

updateItemTitleParser :: Parser ItemTitle
updateItemTitleParser = itemTitleValueParser


updateItemPriorityParser :: Parser ItemPriority
updateItemPriorityParser =
        Just <$> itemPriorityValueParser
    <|> flag' Nothing (long "clear-priority")

updateItemDescriptionParser :: Parser ItemDescription
updateItemDescriptionParser =
        Just <$> itemDescriptionValueParser
    <|> flag' Nothing (long "clear-priority")

updateItemDueByParser :: Parser ItemDueBy
updateItemDueByParser =
        Just <$> itemDueByValueParser
    <|> flag' Nothing (long "clear-due-by")


run :: FilePath -> Command -> IO ()
run filePath comm = case comm of
    Info                   -> print   "Loading Info..."
    Init                   -> print   "Initializing..."
    Add item               -> print $ "Add: item=" <> show item
    List                   -> print   "Listing Items..."
    Remove itemIndex       -> print $ "Removing Item..." <> show itemIndex
    Update idx itemUpdate  -> print $ show idx <> " " <> show itemUpdate
    View item              -> print $ "Loading Item " <> show item
    PathCheck              -> print filePath

main :: IO ()
main =do
    Options dataPath comm <- execParser (info optionsParser (progDesc "To do List Manager"))
    run dataPath comm
