module Hassium.ArgsSpec where

import SpecHelper

spec :: Spec

spec = do
  describe "simplifyOptions" $ do
    it "keep only the last of the overloading flags and the last of the logging enable flags" $
      simplifyOptions [LvmPath "a", LvmPath "a"] `shouldBe` [LvmPath "a"]
    it "keep only the last of the overloading flags and the last of the logging enable flags" $
      simplifyOptions' [LvmPath "a", LvmPath "a"] `shouldBe` [LvmPath "a"]

main :: IO ()
main = hspec spec
