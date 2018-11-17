module Data.Database.TableSpec (spec) where

import Test.Hspec
import Data.Database.Table(Table(..), empty)

-- Hspec example 
-- Usage with stack :: stack test

spec :: Spec
spec = do
  describe "Creating an empty Data.Database.Table" $ do
    
    it "should return an empty Table" $ do
      let emptyTable = empty "Language" [] "The purpose of this table"
      (tableName emptyTable) `shouldBe` "Language"
    
    it "shoud return an empty list of column" $ do
      let emptyTable = empty "Language" [] "The purpose of this table"
      (fields emptyTable) `shouldBe` []