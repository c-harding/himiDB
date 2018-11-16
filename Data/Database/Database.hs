{-# LANGUAGE NamedFieldPuns #-}

module Data.Database.Database(Database(..)) where

import Data.Database.Table(Table(..))
import Data.List(find)

data Database = Database
  { getTables :: [Table]
  }

createTable :: String -> Database -> Database
createTable name db = Database (_ : getTables db)

describeTable :: String -> Database -> Maybe String
describeTable name database = case getTables database of
  [] -> Nothing
  table : ts
    | name == name -> Just ""
    | otherwise    -> Nothing

getTable :: String -> Database -> Maybe Table
getTable string db = find (tableNameIs string) (getTables db)

tableNameIs :: String -> (Table -> Bool)
tableNameIs string (Table {name}) = string == name