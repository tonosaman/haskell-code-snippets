module Examples.TransformersSpec where

import SpecHelper
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "eval0" $ evaluation_context_with eval0
    describe "eval1" $ evaluation_context_with (\ env exp -> runEval1 $ eval1 env exp)
    where
      evaluation_context_with :: (Env -> Exp -> Value) -> Spec
      evaluation_context_with eval = do
        it "evaluate Lit" $
          eval Map.empty (Lit 1) `shouldBe` IntVal 1
        it "evaluate Var" $ let
          env = Map.fromList [("x", IntVal 2)]
          exp = Var "x"
          in eval env exp `shouldBe` IntVal 2
        it "evaluate Plus" $  let
          exp = Lit 1 `Plus` Lit 2
          in eval Map.empty exp `shouldBe` IntVal 3
        it "evaluate Abs" $ let
          exp = Abs "x" $ Var "x" `Plus` Lit 2
          in eval (Map.empty) exp `shouldBe` (FunVal Map.empty "x" $ Var "x" `Plus` Lit 2)
        it "evaluate App" $ let
          exp = App (Abs "x" $ Var "x" `Plus` Lit 2) (Lit 1)
          in eval Map.empty exp `shouldBe` IntVal 3

main :: IO ()
main = hspec spec
