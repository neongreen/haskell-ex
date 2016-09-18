module Expr where

data Expr = Number Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

showExpr :: Expr -> String
showExpr (Number i) = show i
showExpr (Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Sub e1 e2) = showExpr e1 ++ "-" ++ showExpr e2
showExpr (Mul e1 e2) = showExpr e1 ++ "*" ++ showExpr e2
showExpr (Div e1 e2) = showExpr e1 ++ "/" ++ showExpr e2 

evalExpr :: Expr -> Int
evalExpr (Number i) = i
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Sub e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Div e1 e2) = evalExpr e1 `div` evalExpr e2 