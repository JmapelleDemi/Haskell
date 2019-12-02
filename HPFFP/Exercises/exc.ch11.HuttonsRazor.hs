-- Your tas is to write the 'eval' function which reduces an expression to a final sum

data Expr = Lit Integer
          | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x)   = x
eval (Add x y) = (eval x) + (eval y)

-- Write a printer for expressions

printExpr :: Expr -> String
printExpr (Lit x)   = show x
printExpr (Add x y) = (printExpr x) ++ " + " ++ (printExpr y)