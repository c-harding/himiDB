module Data.Database.RecordSpec (spec) where

import Test.Hspec
import Data.Database.Record(Record)

-- Hspec example 
-- Usage with stack :: stack setup && stack test
 
spec :: Spec
spec = do
  describe "Data.Database.Record" $ do
    it "should return an empty Record" $ do
      head [23 ..] `shouldBe` (23 :: Int)    