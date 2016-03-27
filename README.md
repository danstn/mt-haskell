# mt-haskell
Monad Transformers - Step By Step

### Play

```
stack setup
stack ghci

λ: example1
Plus (Lit (-10)) (App (Abs "x" (Plus (Var "x") (Var "x"))) (App (Abs "x" (Var "x")) (Plus (Lit 2) (Lit 2))))

λ: execute example1
-10
2
2
((Right (IntVal (-2)),["x","x","x"]),13)
```
