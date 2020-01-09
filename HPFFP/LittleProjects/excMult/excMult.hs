module ExcMult where

import Test.Hspec

mult :: (Eq a, Num a) => a -> a -> a
mult _ 0 = 0
mult 0 _ = 0
mult 1 b = b
mult a 1 = a 
mult a b = a + (mult a (b - 1))

main :: IO ()
main = hspec $ do
  describe "ExcMult" $ do
    it "100 * 100 is 10000" $ do
      mult 100 100 `shouldBe` 10000
    it "5 * 25 is 125" $ do
      mult 5 25 `shouldBe` 125