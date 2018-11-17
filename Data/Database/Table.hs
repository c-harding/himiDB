{-# LANGUAGE NamedFieldPuns #-}

module Data.Database.Table(Table(tableName, fields), empty, Field, addRecord, Constraint, select, deleteWhere, describe) where

import Data.Database.Record(Record(Record), Type(..), Value(..))
import qualified Data.Database.Record as R
import Data.List(elemIndex, transpose, intercalate)
import Data.Maybe()
data Constraint
  = StrEq Col String
  | IntEq Col Int
  | IntLt Col Int
  | Not Constraint
  | And Constraint Constraint
  | Or Constraint Constraint
  deriving (Show)

type Col = String
type Name = String
type Field = (Col, Type)
type Description = String
 
data Table = Table 
  { tableName :: Name 
  , fields :: [Field]
  , records :: [Record]
  , description :: Description
  }  deriving (Show)

-- Creates an empty Table
empty :: String -> [Field] -> Table
empty name fields = Table name fields [] ""

addRecord :: Record -> Table -> Maybe Table
addRecord record table = Just table{records = record : records table}

filterCols :: [String] -> [Field] -> Maybe [[String]] -> Maybe [[String]]
filterCols selected fields result = filterRow <$> result
  where 
    keepColumn (name, _) = name `elem` selected
    filterRow row = zipFilter (keepColumn <$> fields) row
  
zipFilter :: [Bool] -> [a] -> [a]
zipFilter (True : ts) (x : xs) = x : zipFilter ts xs
zipFilter (False : ts) (_ : xs) = zipFilter ts xs
zipFilter _ _ = []

select :: Constraint -> [String] -> Table -> Maybe [[String]]
select constraints [] table = applyConstraints constraints table
select constraints xs table = filterCols xs (fields table) $ applyConstraints constraints table

deleteWhere :: Constraint -> Table -> Maybe Table
deleteWhere constraints table = do
  predicate <- buildConstraints (fields table) constraints
  let records' = Record <$> filter predicate (R.getValues <$> records table)
  return table{records=records'}

getValues :: Table -> [[Value]]
getValues table = R.getValues <$> records table

applyConstraints  :: Constraint -> Table -> Maybe [[String]]
applyConstraints constraints table = do 
  predicate <- buildConstraints (fields table) constraints
  let values = (filter predicate (getValues table))
  return (map show <$> values)

buildConstraints :: [Field] -> Constraint -> Maybe ([Value] -> Bool)
buildConstraints fields constraints = 
  case constraints of 
    StrEq c v -> do
      i <- elemIndex (c, StringRecord) fields
      return $ conditionAt i (stringMatches (== v))
    IntEq c v -> do 
      i <- elemIndex (c, IntRecord) fields
      return $ conditionAt i (intMatches (== v))
    IntLt c v -> do 
      i <- elemIndex (c, IntRecord) fields
      return $ conditionAt i (intMatches (< v))
    Not con -> (not .) <$> buildConstraints fields con
    And con1 con2 -> combine (&&) <$> buildConstraints fields con1 <*> buildConstraints fields con2
    Or con1 con2 -> combine (||) <$> buildConstraints fields con1 <*> buildConstraints fields con2

combine :: (b -> c -> d) -> (a -> b) -> (a -> c) -> (a -> d)
combine c f g x = f x `c` g x
conditionAt :: Int -> (Value -> Bool) -> [Value] -> Bool
conditionAt i p vs = p (vs !! i)

stringMatches :: (String -> Bool) -> Value -> Bool
stringMatches p (StringValue s) = p s
stringMatches _ _ = False

intMatches :: (Int -> Bool) -> Value -> Bool
intMatches p (IntValue i) = p i
intMatches _ _ = False

describe :: Table -> String
describe table = intercalate " | " (zipWith pad lengths titles) ++ "\n"
                ++ intercalate "-+-" (map (`replicate` '-') lengths) ++ "\n"
                ++ intercalate "\n" (map (intercalate " | " . zipWith pad lengths) values)
  where
    titles = fst <$> fields table

    values = map show <$> getValues table
    lengths = maximum . map length <$> transpose (titles : values)

    pad n [] = replicate n ' '
    pad n (x:xs) = x : pad (pred n) xs