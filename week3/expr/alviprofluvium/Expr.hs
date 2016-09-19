module Expr where

data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr


showExpr :: Expr -> String
showExpr expr = case expr of
  Number n -> if n < 0 then "(" ++ show n ++ ")" else show n
  Add a b  -> showExpr a ++ "+" ++ showExpr b
  Sub a b  -> showExpr a ++ "-" ++ format b
  Mul a b  -> format a   ++ "*" ++ format b
  Div a b  -> format a   ++ "/" ++ parens b
  where
    parens e = case e of
      Number _ -> showExpr e
      _        -> "(" ++ showExpr e ++ ")"
    format e = case e of
      Add _ _  -> "(" ++ showExpr e ++ ")"
      Sub _ _  -> "(" ++ showExpr e ++ ")"
      _        -> showExpr e


evalExpr :: Expr -> Int
evalExpr (Number n) = n
evalExpr (Add a b)  = evalExpr a + evalExpr b
evalExpr (Sub a b)  = evalExpr a - evalExpr b
evalExpr (Mul a b)  = evalExpr a * evalExpr b
evalExpr (Div a b)  = evalExpr a `div` evalExpr b


