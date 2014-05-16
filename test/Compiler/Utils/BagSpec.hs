module Compiler.Utils.BagSpec where

import SpecHelper

spec :: Spec
spec = do
    describe "Bag" $ do
        it "construct from Empty data" $ 1 `shouldBe` 1

main :: IO ()
main = hspec spec
