module Hassium.ArgsSpec where

import SpecHelper

spec :: Spec

spec = do
  describe "simplify" $ do
    it "keep only the last of the overloading flags" $
      simplify [Overloading True, Overloading False] `shouldBe` [LvmPath "", Overloading False]
    it "keep only the last of the overloading flags" $
      simplify [Overloading False, Overloading True] `shouldBe` [LvmPath "", Overloading True]
    it "keep only the last of the logging enable flags" $
      simplify [Logging True, Logging False] `shouldBe` [LvmPath "", Logging False]
    it "keep only the last of the logging enable flags" $
      simplify [Logging False, Logging True] `shouldBe` [LvmPath "", Logging True]
    it "overrides logging flag to turned on by alert flag" $
      simplify [Alert "message"] `shouldSatisfy` (elem $ Logging True)
    it "collects all -P flags together and merges them into one, and the order in which they were specified" $
      simplify [LvmPath "a", LvmPath "b", LvmPath "c:a", LvmPath "d", LvmPath "c"] `shouldBe` [LvmPath "a:b:c:d"]

main :: IO ()
main = hspec spec
