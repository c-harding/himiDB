{-# LANGUAGE NamedFieldPuns #-}

module Data.Database.Table(Table(tableName), empty, Field, addRecord) where

import Data.Database.Record(Record(..))
data Type = IntRecord | StringRecord
type Name = String
type Field = (String, Type)
type Description = String
 
data Table = Table 
  { tableName :: Name 
  , fields :: [Field]
  , records :: [Record]
  }  deriving (Show)

-- Creates an empty Table
empty :: String -> [Field] -> Table
empty name fields = Table name fields []

addRecord :: Record -> Table -> Maybe Table
addRecord record table = Just table{records = record : records table}

select :: [(String, String)] -> [String] -> Maybe [[String]]
select constraints outputs = _
