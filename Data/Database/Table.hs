{-# LANGUAGE NamedFieldPuns #-}

module Data.Database.Table(Table(tableName, fields), empty, Field, addRecord, Constraint, select, deleteWhere, describe) where

import Data.Database.Types
import Data.List(elemIndex, transpose, intercalate)
import Control.Applicative(liftA2)

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

addRecord :: Record -> Table -> Error Table
addRecord record table = checkTypes (fields table) record *> Just table{records = record : records table}

filterCols :: [String] -> [Field] -> Error [[String]] -> Error [[String]]
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
select constraints xs table = filterCols xs (fields table) . applyConstraints constraints $ table

deleteWhere :: Constraint -> Table -> Maybe Table
deleteWhere constraints table = do
  predicate <- buildConstraints (fields table) constraints
  let records' = filter predicate (records table)
  return table{records=records'}

applyConstraints  :: Constraint -> Table -> Maybe [[String]]
applyConstraints constraints table = do 
  predicate <- buildConstraints (fields table) constraints
  let values = (filter predicate (records table))
  return (map show <$> values)

buildConstraints :: [Field] -> Constraint -> Maybe ([Value] -> Bool)
buildConstraints fields constraints = 
  case constraints of 
    StrEq a b -> 
      liftA2 (liftA2 (==))
        (resolveExpr StringRecord fields a)
        (resolveExpr StringRecord fields b)
    IntEq a b -> 
      liftA2 (liftA2 (==))
        (resolveExpr IntRecord fields a)
        (resolveExpr IntRecord fields b)
    IntLt a b ->
      liftA2 (liftA2 (<))
        (resolveExpr IntRecord fields a)
        (resolveExpr IntRecord fields b)
    And con1 con2 ->
      liftA2 (liftA2 (&&))
        (buildConstraints fields con1)
        (buildConstraints fields con2)
    Or con1 con2 ->
      liftA2 (liftA2 (||))
        (buildConstraints fields con1)
        (buildConstraints fields con2)
    Not con -> (not .) <$> buildConstraints fields con

resolveExpr :: ValueClass a => Type -> [Field] -> Either a String -> Maybe ([Value] -> a)
resolveExpr _ _ (Left lit) = Just $ const lit
resolveExpr t fields (Right col) = fmap (getValue) . flip (!!) <$> colIndex (col, t) fields

colIndex :: (String, Type) -> [Field] -> Error Int
colIndex (col, t) fields = checkColType =<< col `elemIndex` (map fst fields)
  where
    checkColType c = if snd (fields !! c) == t then Just c else Nothing

checkType :: Field -> Value -> Error ()
checkType (_, StringRecord) (StringValue _) = Just ()
checkType (_, IntRecord) (IntValue _) = Just ()
checkType _ _ = Nothing

checkTypes :: [Field] -> [Value] -> Error ()
checkTypes [] [] = Just ()
checkTypes (f:fs) (v:vs) = checkType f v *> checkTypes fs vs
checkTypes _ _ = Nothing

describe :: Table -> String
describe table = intercalate " | " (zipWith pad lengths titles) ++ "\n"
                ++ intercalate "-+-" (map (`replicate` '-') lengths) ++ "\n"
                ++ intercalate "\n" (map (intercalate " | " . zipWith pad lengths) values)
  where
    titles = fst <$> fields table

    values = map show <$> records table
    lengths = maximum . map length <$> transpose (titles : values)

    pad n [] = replicate n ' '
    pad n (x:xs) = x : pad (pred n) xs