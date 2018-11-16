module Data.Database.Record where

type FieldValues = [(String, String)]

newtype Fields = 
    Fields {
        unfields :: String
    } deriving (Show, Eq)

data Record = 
    Record {
          getFlds   :: Fields 
        , getFldVls :: FieldValues
    } deriving (Show, Eq)

createRecord :: Fields -> FieldValues -> Record
createRecord = Record

selectFromRecord :: String -> Record -> Maybe String
selectFromRecord qry = lookup qry . getFldVls
