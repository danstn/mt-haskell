{-# LANGUAGE FlexibleContexts #-}

module Transformers where

import           Control.Monad.Identity
import           Control.Monad.Error
import           Control.Monad.Reader

import           Data.Maybe
import qualified Data.Map as Map

-- Variable names
type Name = String

-- Expressions
data Exp = Lit Integer  -- constants
         | Var Name     -- variables
         | Plus Exp Exp -- addition
         | Abs Name Exp -- abstractions (lambda)
         | App Exp Exp  -- function application
         deriving (Show)

-- Values
data Value = IntVal Integer
           | FunVal Env Name Exp -- closure
           deriving (Show)

-- Mapping from name to values
type Env = Map.Map Name Value

envEmpty :: Env
envEmpty = Map.empty

envInsert :: Name -> Value -> Env -> Env
envInsert = Map.insert

envLookup :: Name -> Env -> Maybe Value
envLookup = Map.lookup

-- Programs will be made out of expressions; results - out of values;
--------------------------------------------------------------------------------
eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (envLookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                          in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                         in case val1 of
                              FunVal env' n body -> eval0 (envInsert n val2 env') body


-- Monadic version
--------------------------------------------------------------------------------
type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Monad m => Env -> Exp -> m Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust $ envLookup n env
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                             FunVal env' n body ->
                               eval1 (envInsert n val2 env') body


-- ErrorT
--------------------------------------------------------------------------------
type ErrorMsg = String
type Eval2 a = ErrorT ErrorMsg Identity a

runEval2 :: Eval2 a -> Either ErrorMsg a
runEval2 = runIdentity . runErrorT

-- Without error catching
eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = return $ fromJust $ envLookup n env
eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1
                             IntVal i2 <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                              FunVal env' n body ->
                                eval2a (envInsert n val2 env') body

-- Note: FlexibleContexts extension allows this to be used under any monad
-- i.e Eval2 & Eval3
errUnbound n = throwError $ "Unbound variable: " ++ n

-- With error catching
eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = maybe (errUnbound n) return $ envLookup n env
eval2b env (Plus e1 e2) = do e1' <- eval2b env e1
                             e2' <- eval2b env e2
                             case (e1', e2') of
                               (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                               _ -> throwError "Type error in addition"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do val1 <- eval2b env e1
                            val2 <- eval2b env e2
                            case val1 of
                              FunVal env' n body -> eval2b (envInsert n val2 env') body
                              _ -> throwError "Type error in application"

eval2 = eval2b

-- ReaderT
--------------------------------------------------------------------------------
type Eval3 a = ReaderT Env (ErrorT String Identity) a

runEval3 :: Env -> Eval3 a -> Either ErrorMsg a
runEval3 env ev = runIdentity $ runErrorT $ runReaderT ev env

-- Hiding the environment
eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = ask >>= \env -> maybe (errUnbound n) return $ envLookup n env
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                          (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                          _ -> throwError "Type error in addition"
eval3 (Abs n e) = ask >>= \env -> return $ FunVal env n e
eval3 (App e1 e2) = do val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                         FunVal env' n body ->
                           local (const (envInsert n val2 env')) (eval3 body)
                         _ -> throwError "Type error in application"

-- Example expression:
-- 10 + ((\x -> x)(5 + 8))
-- IntVal 23
exampleExp :: Exp
exampleExp = Lit 10 `Plus` (App (Abs "x" (Var "x")) (Lit 5 `Plus` Lit 8))

