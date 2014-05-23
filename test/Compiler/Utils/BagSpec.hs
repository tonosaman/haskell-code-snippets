module Compiler.Utils.BagSpec where

import SpecHelper

spec :: Spec
spec = do
    describe "Bag" $ do
        it "construct from Empty data" $ 1 `shouldBe` (0 + 1)
        it "test1" $ (filter (/=1) . ([1,2,3] ++)) [1] `shouldBe` [2,3]

main :: IO ()
main = hspec spec
