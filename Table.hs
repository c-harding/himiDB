module Data.Database.Table where

type Id = Int
type Field = [String] 
type Record = (String, String)
type Description = String
    
data Table = Table {
    id :: Id 
    fields :: Field
    records :: Record
    description :: Description
}

