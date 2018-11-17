{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Database.Types (Type(..), Value(..), Constraint(..), Error, Record, ValueClass(..)) where

data Type = IntRecord | StringRecord deriving (Show, Eq)

type Record = [Value]

data Value = IntValue { getIntValue :: Int } | StringValue { getStringValue :: String } deriving (Eq)

class ValueClass a where
  getValue :: Value -> a
  mkValue :: a -> Value

instance ValueClass String where
  getValue = getStringValue
  mkValue = StringValue

instance ValueClass Int where
  getValue = getIntValue
  mkValue = IntValue

-- getValue :: Type -> Value -> a
-- getValue IntRecord = getIntValue
-- getValue StringRecord = getStringValue

-- mkValue :: Type -> a -> Value
-- mkValue IntRecord = IntValue
-- mkValue StringRecord = StringValue

type Error = Maybe
type ErrorReport = ()

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