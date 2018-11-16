module Data.Database.Record where

import qualified Data.Map as M

type FieldValues = M.Map String String

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
createRecord flds fldVls = Record flds fldVls

selectFromRecord :: String -> Record -> Maybe String
selectFromRecord qry rcd = M.lookup qry (getFldVls rcd)
