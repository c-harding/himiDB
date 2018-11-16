import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

-- Hspec example 
-- Usage with stack :: stack setup && stack test
 
main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)
