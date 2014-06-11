module Examples.TransformersSpec where

import SpecHelper
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "eval0" $ evaluation_context_with eval0
    describe "eval1" $ evaluation_context_with (\ ev ex -> runEval1 $ eval1 ev ex)
    describe "eval2" $ evaluation_context_with . normarize $ \ ev ex -> runEval2 $ eval2 ev ex
    describe "eval3" $ evaluation_context_with . normarize $ \ ev ex -> runEval3 (eval3 ex) ev
    describe "eval4" $ evaluation_context_with . normarize $ \ ev ex -> fst $ runEval4 (eval4 ex) ev 0
    where
      normarize :: (Env -> Exp -> Either a Value) -> (Env -> Exp -> Value)
      normarize runEval = \ev -> either (const $ IntVal 0) id . runEval ev
      evaluation_context_with :: (Env -> Exp -> Value) -> Spec
      evaluation_context_with eval = do
        it "evaluate Lit" $
          eval Map.empty (Lit 1) `shouldBe` IntVal 1
        it "evaluate Var" $ let
          ev = Map.fromList [("x", IntVal 2)]
          ex = Var "x"
          in eval ev ex `shouldBe` IntVal 2
        it "evaluate Plus" $  let
          ex = Lit 1 `Plus` Lit 2
          in eval Map.empty ex `shouldBe` IntVal 3
        it "evaluate Abs" $ let
          ex = Abs "x" $ Var "x" `Plus` Lit 2
          in eval (Map.empty) ex `shouldBe` (FunVal Map.empty "x" $ Var "x" `Plus` Lit 2)
        it "evaluate App" $ let
          ex = App (Abs "x" $ Var "x" `Plus` Lit 2) (Lit 1)
          in eval Map.empty ex `shouldBe` IntVal 3

main :: IO ()
main = hspec spec
