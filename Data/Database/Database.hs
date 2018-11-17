module Data.Database.Database(Database, empty, createTable, insertRecord, describeTable, deleteTable, deleteWhere, select) where

import           Data.Database.Types
import           Data.Database.Table(Table(tableName), Constraint)
import qualified Data.Database.Table as T
import Data.List(find)

type Database = [Table]

empty :: Database
empty = []

createTable :: String -> [T.Field] -> String -> Database -> Database
createTable name fields description db = (T.empty name fields description: db)

insertRecord :: String -> Record -> Database -> Maybe Database
insertRecord name record = liftUpdate (`tableNameIs` name) (T.addRecord record)

describeTable :: String -> Database -> Maybe String
describeTable name db = T.describe <$> getTable name db

deleteTable :: String -> Database -> Maybe Database
deleteTable _ [] = Nothing
deleteTable name (t:ts)
  | t `tableNameIs` name = Just ts
  | otherwise            = (t:) <$> deleteTable name ts

select :: String -> Constraint -> [String] -> Database -> Maybe [[String]]
select name constraints outputs db = T.select constraints outputs =<< getTable name db

deleteWhere :: String -> Constraint -> Database -> Maybe Database
deleteWhere name constraints = liftUpdate (`tableNameIs` name) (T.deleteWhere constraints)

getTable :: String -> Database -> Maybe Table
getTable name = find (`tableNameIs` name)

tableNameIs :: Table -> String -> Bool
table `tableNameIs` name = name == tableName table

update :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
update _ _ []     = Nothing
update p f (x:xs)
  | p x           = Just $ f x : xs
  | otherwise     = (x :) <$> update p f xs

liftUpdate :: (a -> Bool) -> (a -> Maybe a) -> [a] -> Maybe [a]
liftUpdate _ _ []     = Nothing
liftUpdate p f (x:xs)
  | p x               = (: xs) <$> f x
  | otherwise         = (x :) <$> liftUpdate p f xs