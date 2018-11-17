module Data.Database.DatabaseMonad(DatabaseMonad, D.empty, createTable, insertRecord, describeTable, deleteTable, select) where

import           Control.Monad.State.Strict(State, modify, get, put)

import           Data.Database.Types
import qualified Data.Database.Table as T
import qualified Data.Database.Database as D

type DatabaseMonad a = State D.Database a

createTable :: String -> [T.Field] -> DatabaseMonad ()
createTable name fields = modify (D.createTable name fields)

insertRecord :: String -> Record -> DatabaseMonad Bool
insertRecord name record = maybeModify (D.insertRecord name record)

describeTable :: String -> DatabaseMonad (Maybe String)
describeTable name = D.describeTable name <$> get

deleteTable :: String -> DatabaseMonad Bool
deleteTable name = maybeModify (D.deleteTable name)

select :: String -> T.Constraint -> [String] -> DatabaseMonad (Maybe [[String]])
select name constraints outputs = D.select name constraints outputs <$> get

maybeModify :: (D.Database -> Maybe D.Database) -> DatabaseMonad Bool
maybeModify f = maybe (return False) ((True <$) . put) . f =<< get