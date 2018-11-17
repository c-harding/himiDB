module Data.Database.Database(Database, empty, createTable, insertRecord, describeTable, deleteTable, deleteWhere, select, showTables) where

import           Data.Database.Types
import           Data.Database.Table(Table(tableName), Constraint, showTable)
import qualified Data.Database.Table as T
import Data.List(find, intercalate)

type Database = [Table]

empty :: Database
empty = []

createTable :: String -> [Field] -> String -> Database -> Database
createTable name fields description db = (T.empty name fields description: db)

insertRecord :: String -> Record -> Database -> Error Database
insertRecord name record = updateTable name (T.addRecord record)

describeTable :: String -> Database -> Error String
describeTable name db = T.describe <$> getTable name db

deleteTable :: String -> Database -> Error Database
deleteTable name [] = throwError ("Table not found: "++name)
deleteTable name (t:ts)
  | t `tableNameIs` name = noError ts
  | otherwise            = (t:) <$> deleteTable name ts

select :: String -> Constraint -> [String] -> Database -> Error [[String]]
select name constraints outputs db = T.select constraints outputs =<< getTable name db

deleteWhere :: String -> Constraint -> Database -> Error Database
deleteWhere name constraints = updateTable name (T.deleteWhere constraints)

getTable :: String -> Database -> Error Table
getTable name = orError ("Table not found: "++name) . find (`tableNameIs` name)

tableNameIs :: Table -> String -> Bool
table `tableNameIs` name = name == tableName table

updateTable :: String -> (Table -> Error Table) -> [Table] -> Error [Table]
updateTable name f = update ("Table not found: "++name) (`tableNameIs` name) f

showTables :: Database -> String
showTables db = intercalate "\n" $ showTable <$> db

update :: ErrorReport -> (a -> Bool) -> (a -> Error a) -> [a] -> Error [a]
update msg _ _ []     = throwError (show msg)
update msg p f (x:xs)
  | p x               = (: xs) <$> f x
  | otherwise         = (x :) <$> update msg p f xs