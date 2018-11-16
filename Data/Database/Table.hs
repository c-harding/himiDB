module Data.Database.Table(Table(..)) where

import Data.Database.Record(Record(..))
type Name = String
type Field = [String] 
type Description = String
    
data Table = Table 
  { name :: Name 
  , fields :: Field
  , records :: Record
  , description :: Description
  }

-- class TableTypeClass a where
--   insert :: a -> Table -> Table
--   select :: a -> Table -> Table
--   delete :: a -> Table -> Table
--   description :: a -> Table -> Table
