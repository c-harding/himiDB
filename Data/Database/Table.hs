module Data.Database.Table where

type Id = String
type Field = [String] 
type Record = (String, String)
type Description = String
    
data Table = Table 
  { id :: Id 
  , fields :: Field
  , records :: Record
  , description :: Description
  }

class TableTypeClass a where
  insert :: a -> Table -> Table
  select :: a -> Table -> Table
  delete :: a -> Table -> Table
  description :: a -> Table -> Table
  