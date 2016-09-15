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
showExpr (Add a b) = parens a ++ "+" ++ parens b
  where 
  parens a@(Sub _ _) = "(" ++ showExpr a ++ ")"
  parens a = showExpr a
showExpr (Sub a b) = parens a ++ "-" ++ parens b
  where 
  parens a@(Add _ _) = "(" ++ showExpr a ++ ")"
  parens a = showExpr a
showExpr (Mul a b) = parens a ++ "*" ++ parens b
  where
  parens a@(Add _ _) = "(" ++ showExpr a ++ ")"
  parens a@(Sub _ _) = "(" ++ showExpr a ++ ")"
  parens a = showExpr a
showExpr (Div a b) = parens a ++ "/" ++ parens b
  where
  parens a@(Add _ _) = "(" ++ showExpr a ++ ")"
  parens a@(Sub _ _) = "(" ++ showExpr a ++ ")"
  parens a@(Div _ _) = "(" ++ showExpr a ++ ")"
  parens a = showExpr a


evalExpr :: Expr -> Int
evalExpr (Number n) = n
evalExpr (Add a b)  = evalExpr a + evalExpr b
evalExpr (Sub a b)  = evalExpr a - evalExpr b
evalExpr (Mul a b)  = evalExpr a * evalExpr b
evalExpr (Div a b)  = evalExpr a `div` evalExpr b


