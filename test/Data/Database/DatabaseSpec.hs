module Data.Database.DatabaseSpec (spec) where

import Test.Hspec
import Data.Database.Database(Database(..), empty, createTable, showTables)
import Data.Database.Table(Table(..))

-- Hspec example 
-- Usage with stack :: stack test
-- aData = Data.Database.Database.empty 
-- aTable = Data.Database.Database.createTable "Table_1" [] "Desc" aData 

spec :: Spec
spec = do
  describe "Module :: Data.Database.Database" $ do
    it "Create an empty Database should an database without Tables" $ do
      let anEmptyDatabase = empty
      -- let databaseWithTable = createTable "TableName" [] "PurposeOfTable" anEmptyDatabase
      (showTables anEmptyDatabase) `shouldBe` ""