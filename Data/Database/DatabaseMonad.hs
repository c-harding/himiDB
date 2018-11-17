{-# LANGUAGE FlexibleContexts #-}

module Data.Database.DatabaseMonad(D.Database, D.empty, createTable, insertRecord, describeTable, deleteTable, select, deleteWhere, showTables) where

import           Control.Monad.State.Strict(MonadState, modify, get, put)

import           Data.Database.Types
import qualified Data.Database.Table as T
import qualified Data.Database.Database as D

createTable :: MonadState D.Database db => String -> [Field] -> String -> db ()
createTable name fields description = modify (D.createTable name fields description)

insertRecord :: MonadState D.Database db => String -> Record -> db (Maybe ErrorReport)
insertRecord name record = maybeModify (D.insertRecord name record)

describeTable :: MonadState D.Database db => String -> db (Error String)
describeTable name = D.describeTable name <$> get

showTables :: MonadState D.Database db => db String
showTables = D.showTables <$> get

deleteTable :: MonadState D.Database db => String -> db (Maybe ErrorReport)
deleteTable name = maybeModify (D.deleteTable name)

select :: MonadState D.Database db => String -> T.Constraint -> [String] -> db (Error [[String]])
select name constraints outputs = D.select name constraints outputs <$> get

deleteWhere :: MonadState D.Database db => String -> T.Constraint -> db (Maybe ErrorReport)
deleteWhere name constraints = maybeModify (D.deleteWhere name constraints)

maybeModify :: MonadState D.Database db => (D.Database -> Error D.Database) -> db (Maybe ErrorReport)
maybeModify f = either (return . Just) ((Nothing <$) . put) . f =<< get