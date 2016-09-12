module Expr where

data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

showExpr :: Expr -> String
showExpr (Number n)
  | n < 0     = "(" ++ show n ++ ")"
  | otherwise = show n
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Sub a b) = showExpr a ++ "-" ++ showExpr b
showExpr (Mul a b) = showExpr a ++ "*" ++ showExpr b
showExpr (Div a b) = showExpr a ++ "/" ++ showExpr b

evalExpr :: Expr -> Int
evalExpr (Number n) = n
evalExpr (Add a b)  = evalExpr a + evalExpr b
evalExpr (Sub a b)  = evalExpr a - evalExpr b
evalExpr (Mul a b)  = evalExpr a * evalExpr b
evalExpr (Div a b)  = evalExpr a `div` evalExpr b


