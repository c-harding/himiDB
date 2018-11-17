module Data.Database.Types (Type(..), Value(..), Constraint(..), Error, Record, getValues) where

data Type = IntRecord | StringRecord deriving (Show, Eq)

type Record = [Value]
getValues :: Record -> Record
getValues = id

data Value = IntValue { getIntValue :: Int } | StringValue { getStringValue :: String } deriving (Eq)

type Error = Maybe

instance Show Value where
    show (IntValue x) = show x
    show (StringValue s) = show s
  
type StrExpr = Either String String
type IntExpr = Either Int String
data Constraint
  = StrEq StrExpr StrExpr
  | IntEq IntExpr IntExpr
  | IntLt IntExpr IntExpr
  | Not Constraint
  | And Constraint Constraint
  | Or Constraint Constraint
  deriving (Show)