{-# LANGUAGE NamedFieldPuns #-}

module Data.Database.Table(Table(tableName, fields), empty, Field, addRecord, Constraint) where

import Data.Database.Record(Record(..), Type(..))
data Constraint
  = StrEq Col String
  | IntEq Col Int
  | IntLt Col Int
  | Not Constraint
  | And [Constraint]
  | Or [Constraint]
  deriving (Show)

type Col = String
type Name = String
type Field = (Col, Type)
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

select :: Constraint -> [String] -> Maybe [[String]]
select constraints outputs = _
