{-# LANGUAGE NamedFieldPuns #-}

module Data.Database.Table(Table(tableName, fields), empty, addRecord, Constraint, select, deleteWhere, describe, showTable) where

import Data.Database.Types

import Data.List           (elemIndex, transpose, intercalate)
import Control.Applicative (liftA2)
 
data Table = Table 
  { tableName :: Name 
  , fields :: [Field]
  , records :: [Record]
  , description :: Description
  }  deriving (Show)

-- Creates an empty Table
empty :: String -> [Field] -> String -> Table
empty name fields description = Table name fields [] description

addRecord :: Record -> Table -> Error Table
addRecord record table = checkTypes (fields table) record *> noError table{records = record : records table}

filterCols :: [String] -> [Field] -> Error [[a]] -> Error [[a]]
filterCols selected fields result = map filterRow <$> result
  where 
    keepColumn (name, _) = name `elem` selected
    filterRow row = zipFilter (keepColumn <$> fields) row
  
zipFilter :: [Bool] -> [a] -> [a]
zipFilter (True : ts) (x : xs) = x : zipFilter ts xs
zipFilter (False : ts) (_ : xs) = zipFilter ts xs
zipFilter _ _ = []

select :: Constraint -> [String] -> Table -> Error [[Value]]
select constraints [] table = applyConstraints constraints table
select constraints xs table = filterCols xs (fields table) . applyConstraints constraints $ table

deleteWhere :: Constraint -> Table -> Error Table
deleteWhere constraints table = do
  predicate <- buildConstraints (fields table) constraints
  let records' = filter predicate (records table)
  return table{records=records'}

applyConstraints  :: Constraint -> Table -> Error [[Value]]
applyConstraints constraints table = do 
  predicate <- buildConstraints (fields table) constraints
  return (filter predicate (records table))

buildConstraints :: [Field] -> Constraint -> Error ([Value] -> Bool)
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
    Not con ->
      (not .) <$> buildConstraints fields con
    All -> noError (const True)

resolveExpr :: ValueClass a => Type -> [Field] -> Either a String -> Error ([Value] -> a)
resolveExpr _ _ (Left lit) = noError $ const lit
resolveExpr t fields (Right col) = fmap (getValue) . flip (!!) <$> colIndex (col, t) fields

colIndex :: (String, Type) -> [Field] -> Error Int
colIndex (col, t) fields = checkColType =<< orError ("No such column "++col) (col `elemIndex` (map fst fields))
  where
    checkColType c
      | snd (fields !! c) == t = noError c
      | otherwise              = throwError ("Column "++col++" is of type "++show t++", not "++show (fields !! c))

checkType :: Field -> Value -> Error ()
checkType (_, StringRecord) (StringValue _) = noError ()
checkType (_, IntRecord) (IntValue _) = noError ()
checkType (c, _) _ = throwError ("Wrong type for column "++c)

checkTypes :: [Field] -> [Value] -> Error ()
checkTypes [] [] = noError ()
checkTypes (f:fs) (v:vs) = checkType f v *> checkTypes fs vs
checkTypes _ _ = throwError "Wrong number of columns provided"

describe :: Table -> String
describe table = desc ++ drawTable (Just [titles, types]) (records table)
  where
    desc = tableName table ++ ": " ++ description table ++ "\n"
    titles = fst <$> fields table
    types = show . snd <$> fields table

showTable :: Table -> String
showTable t = tableName t ++ " (" ++ (intercalate ", " $ showField <$> fields t) ++ ") " ++ description t
  where
    showField (n, ty) = n ++ " " ++ show ty