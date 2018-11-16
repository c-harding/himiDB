module Test.Data.Database.RecordSpec where

import Test.Hspec
import Data.Database.Record(Record)

-- Hspec example 
-- Usage with stack :: stack setup && stack test
 
main :: IO ()
main = hspec $ do
  describe "Data.Database.Record" $ do
    it "should return an empty Record" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    --it "returns the first element of an *arbitrary* list" $
      --property $ \x xs -> head (x:xs) == (x :: Int)
    