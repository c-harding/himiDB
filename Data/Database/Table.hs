{-# LANGUAGE NamedFieldPuns #-}

module Data.Database.Table(Table(tableName), empty, Field, addRecord) where

import Data.Database.Record(Record(..))
type Name = String
type Field = String
    
data Table = Table 
  { tableName :: Name 
  , fields :: [Field]
  , records :: [Record]
  }

empty :: String -> [Field] -> Table
empty name fields = Table name fields []

addRecord :: Record -> Table -> Maybe Table
addRecord record table = Just table{records = record : records table}

-- class TableTypeClass a where
--   insert :: a -> Table -> Table
--   select :: a -> Table -> Table
--   delete :: a -> Table -> Table

select :: [(String, String)] -> [String] -> Maybe [[String]]
select constraints outputs = _