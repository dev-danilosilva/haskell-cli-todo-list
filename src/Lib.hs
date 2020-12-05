module Lib where

data Todo = Todo
    { title :: String
    , done  :: Bool
    }

todoList :: [Todo]
todoList = []