module Examples.Transformers
       ( Name
       , Exp(Lit,Var,Plus,Abs,App)
       , Value(IntVal,FunVal)
       , Env
       , eval0
       , eval1, runEval1
       , eval2, runEval2
       , eval3, runEval3
       ) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Name = String
data Exp
  = Lit Integer
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving (Show, Eq)
data Value
  = IntVal Integer
  | FunVal Env Name Exp
  deriving (Show, Eq)
type Env = Map.Map Name Value

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust $ Map.lookup n env
eval0 env (Plus e1 e2) = 
  let IntVal i1 = eval0 env e1
      IntVal i2 = eval0 env e2
  in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = 
  let FunVal env' n e = eval0 env e1
      iv@(IntVal _) = eval0 env e2
  in eval0 (Map.insert n iv env') e

{-- convert to Identity Monad -}

type Eval1 = Identity
runEval1 = runIdentity

eval1 :: Env -> Exp -> Eval1 Value
eval1 env arg = case arg of
  Lit i -> return $ IntVal i
  Var n -> return $ fromJust $ Map.lookup n env
  {--
  Plus e1 e2 -> do
    IntVal i1 <- eval1 env e1
    IntVal i2 <- eval1 env e2
    return $ IntVal(i1 + i2)
  --}
  -- vanilla monadic style
  Plus e1 e2 ->
    eval1 env e1 >>= \ iv1 -> 
    eval1 env e2 >>= \ iv2 ->
    case (iv1, iv2) of (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
  Abs n e -> return $ FunVal env n e
  {--
  App e1 e2 -> do
    FunVal env' n body <- eval1 env e1 
    iv@(IntVal _) <- eval1 env e2
    eval1 (Map.insert n iv env') body
   --}
  -- vanilla monadic style
  App e1 e2 ->
    eval1 env e1 >>= \ fv ->
    eval1 env e2 >>= \ iv ->
    case fv of (FunVal env' n body) -> eval1 (Map.insert n iv env') body

{-- add error handring by ErrorT --}
type Eval2 = ErrorT String Identity
runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runErrorT
                            
eval2 :: Env -> Exp -> Eval2 Value
eval2 env exp = case exp of
  Lit i -> return $ IntVal i
  Var n -> maybe (throwError $ "Var " ++ n ++ " not found") return (Map.lookup n env)
  -- Var n -> maybe (fail $ "ERROR: Var " ++ n ++ " not found") return (Map.lookup n env) 
  Plus e1 e2 ->
    eval2 env e1 >>= \ iv1 -> 
    eval2 env e2 >>= \ iv2 ->
    case (iv1, iv2) of
      (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
      otherwise -> throwError $ "Invalid Plus operand(s)."
  Abs n e -> return $ FunVal env n e
  App e1 e2 ->
    eval2 env e1 >>= \ fv ->
    eval2 env e2 >>= \ iv ->
    case fv of
      (FunVal env' n body) -> eval2 (Map.insert n iv env') body
      otherwise -> throwError "Invalid App operand(s)."

{-- add environment by ReaderT --}
type Eval3 = ReaderT Env (ErrorT String Identity)
runEval3 :: Eval3 a -> Env -> Either String a
runEval3 env = runIdentity . runErrorT . runReaderT env

eval3 :: Exp -> Eval3 Value
eval3 exp = case exp of
  Lit i -> return $ IntVal i
  Var n ->
    ask >>= \ env ->
    maybe (throwError $ "Var " ++ n ++ " not found") return (Map.lookup n env)
  Plus e1 e2 ->
    ask >>= \ env ->
    eval3 e1 >>= \ iv1 -> 
    eval3 e2 >>= \ iv2 ->
    case (iv1, iv2) of
      (IntVal i1, IntVal i2) -> return $ IntVal(i1 + i2)
      otherwise -> throwError $ "Invalid Plus operand(s)."
  Abs n e -> ask >>= \ env -> return $ FunVal env n e
  App e1 e2 ->
    eval3 e1 >>= \ fv ->
    eval3 e2 >>= \ iv ->
    case fv of
      (FunVal env' n body) -> local (Map.insert n iv) $ eval3 body
      otherwise -> throwError "Invalid App operand(s)."
