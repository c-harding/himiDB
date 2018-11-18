{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Database.Types(
  Type(..), Value(..), Constraint(..), ValueClass(..), IntExpr(..), StrExpr(..),
  Field, Col, Name, Description, Error, ErrorReport, Record,
  noError, throwError, orError) where

data Type = IntRecord | StringRecord deriving (Eq)
instance Show Type where
  show IntRecord = "int"
  show StringRecord = "string"

type Record = [Value]

type Col = String
type Name = String
type Description = String
type Field = (Col, Type)

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

type Error = Either ErrorReport
type ErrorReport = String

instance Show Value where
    show (IntValue x) = show x
    show (StringValue s) = show s
  
type StrExpr = Either String String
type IntExpr = Either Int String
data Constraint
  = All
  | StrEq StrExpr StrExpr
  | IntEq IntExpr IntExpr
  | IntLt IntExpr IntExpr
  | Not Constraint
  | And Constraint Constraint
  | Or Constraint Constraint
  deriving (Show)

noError :: a -> Error a
noError = Right

throwError :: String -> Error a
throwError = Left

orError :: String -> Maybe a -> Error a
orError msg = maybe (throwError msg) Right
