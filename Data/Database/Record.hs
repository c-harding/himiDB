module Data.Database.Record where

type Field       = String
type FieldValues = [(String, String)]

newtype Fields = 
    Fields {
        unfields :: String
    } deriving (Show, Eq)

data Record = 
    Record {
          getFields      :: Fields 
        , getFieldValues :: FieldValues
    } deriving (Show, Eq)

newtype Predicate = 
    Predicate {
        unPredicate :: String -> Bool
    }

createRecord :: Fields -> FieldValues -> Record
createRecord = Record

selectFromRecord :: Field -> Record -> Maybe String
selectFromRecord field = lookup field . getFieldValues

-- apply a predicate on the fieldvalues
filterRecord :: Predicate -> Record -> FieldValues
filterRecord (Predicate p) = filter (p . snd) . getFieldValues
