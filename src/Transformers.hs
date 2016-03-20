module Transformers where

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

-- Programs will be made out of expressions; results - out of values;

eval0 :: Env -> Exp -> Value
eval0 env (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                          in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                         in case val1 of
                              FunVal env' n body -> eval0 (Map.insert n val2 env') body

-- Example expression:
-- 10 + ((\x -> x)(5 + 8))
-- IntVal 23
exampleExp = Lit 10 `Plus` (App (Abs "x" (Var "x")) (Lit 5 `Plus` Lit 8))


