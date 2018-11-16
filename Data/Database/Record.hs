module Data.Database.Record (Type(..), Value(..), Record(..)) where

data Type = IntRecord | StringRecord deriving (Show, Eq)

data Value = IntValue Int | StringValue String deriving (Show, Eq)

data Record = Record
  { getValues :: [Value]
  } deriving (Show, Eq)

-- createRecord :: Fields -> FieldValues -> Record
-- createRecord = Record

-- selectFromRecord :: Field -> Record -> Maybe String
-- selectFromRecord field = lookup field . getFieldValues

-- -- apply a predicate on the fieldvalues
-- filterRecord :: (String -> Bool) -> Record -> FieldValues
-- filterRecord p = filter (p . snd) . getFieldValues
