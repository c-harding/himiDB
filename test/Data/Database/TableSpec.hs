module Data.Database.TableSpec (spec) where

import Test.Hspec
import Data.Database.Table(Table(..))

-- Hspec example 
-- Usage with stack :: stack test

spec :: Spec
spec = do
  describe "Data.Database.Table" $ do
    it "should return an empty Table" $ do
      head [23 ..] `shouldBe` (23 :: Int)    