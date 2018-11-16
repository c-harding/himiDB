{-# LANGUAGE NamedFieldPuns #-}

module Data.Database.Table(Table(tableName, fields), empty, Field, addRecord, Constraint, select) where

import Data.Database.Record(Record, Type(..), Value(..))
import qualified Data.Database.Record as R
import Data.List(elemIndex)
import Data.Maybe(catMaybes)
data Constraint
  = StrEq Col String
  | IntEq Col Int
  | IntLt Col Int
  | Not Constraint
  -- | And [Constraint]
  -- | Or [Constraint]
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

filterCols :: [String] -> [Field] -> Maybe [[String]] -> Maybe [[String]]
filterCols selected fields result = filterRow <$> result
  where 
    keepColumn (name, _) = name `elem` selected
    keptFlags = keepColumn <$> fields
    keep elem flag = if flag then Just elem else Nothing
    filterRow row = catMaybes (zipWith keep row keptFlags)
  

select :: Constraint -> [String] -> Table -> Maybe [[String]]
select constraints [] table = applyConstraints constraints table
select constraints xs table = filterCols xs (fields table) $ applyConstraints constraints table

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
    -- And cons -> _ <$> sequence (buildConstraints fields <$> cons)
    -- Or cons -> ((any .) . (<$>)) <$> sequence (buildConstraints fields <$> cons)
conditionAt :: Int -> (Value -> Bool) -> [Value] -> Bool
conditionAt i p vs = p (vs !! i)

stringMatches :: (String -> Bool) -> Value -> Bool
stringMatches p (StringValue s) = p s
stringMatches p _ = False

intMatches :: (Int -> Bool) -> Value -> Bool
intMatches p (IntValue i) = p i
intMatches p _ = False