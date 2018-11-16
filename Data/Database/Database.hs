{-# LANGUAGE NamedFieldPuns #-}

module Data.Database.Database(Database) where

import           Data.Database.Table(Table(tableName))
import           Data.Database.Record(Record)
import qualified Data.Database.Table as T
import Data.List(find)

type Database = [Table]

createTable :: String -> [T.Field] -> Database -> Database
createTable name fields db = (T.empty name fields : db)

insertRecord :: String -> Record -> Database -> Maybe Database
insertRecord name record db = liftUpdate (`tableNameIs` name) (T.addRecord record) db

describeTable :: String -> Database -> Maybe String
describeTable name db = tableName <$> getTable name db

deleteTable :: String -> Database -> Maybe Database
deleteTable name [] = Nothing
deleteTable name (t:ts)
  | t `tableNameIs` name = Just ts
  | otherwise            = (t:) <$> deleteTable name ts

select :: String -> [(String, String)] -> [String] -> Maybe [[String]]
select name constraints outputs = T.select constraints outputs <$> getTable name

getTable :: String -> Database -> Maybe Table
getTable name = find (`tableNameIs` name)

tableNameIs :: Table -> String -> Bool
table `tableNameIs` name = name == tableName table

update :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
update p f []     = Nothing
update p f (x:xs)
  | p x           = Just $ f x : xs
  | otherwise     = (x :) <$> update p f xs

liftUpdate :: (a -> Bool) -> (a -> Maybe a) -> [a] -> Maybe [a]
liftUpdate p f []     = Nothing
liftUpdate p f (x:xs)
  | p x               = (: xs) <$> f x
  | otherwise         = (x :) <$> liftUpdate p f xs