data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  
showExpr :: Expr -> String
showExpr expr = case expr of
    Add e1 e2 -> showExpr  e1 ++ "+" ++ showExpr  e2
    Sub e1 e2 -> showExpr  e1 ++ "/" ++ showExpr  e2
    Mul e1 e2 -> parenExpr e1 ++ "*" ++ parenExpr e2
    Div e1 e2 -> parenExpr e1 ++ "*" ++ parenExpr e2
    Number n  -> if n < 0 then "(" ++ show n ++ ")" else show n
  where
    parenExpr expr' = case expr' of
      Add _ _ -> "(" ++ showExpr expr' ++ ")"
      Sub _ _ -> "(" ++ showExpr expr' ++ ")"
      _         ->        showExpr expr'
      
evalExpr :: Expr -> Int      
evalExpr expr = case expr of
  Add e1 e2 -> evalExpr e1 + evalExpr e2
  Sub e1 e2 -> evalExpr e1 - evalExpr e2
  Mul e1 e2 -> evalExpr e1 * evalExpr e2
  Div e1 e2 -> evalExpr e1 `div` evalExpr e2
  Number n  -> n
  
main :: IO ()
main = do
  print $ showExpr (Mul (Number 3) (Add (Number 5) (Number 7)))
  print $ evalExpr (Mul (Number 3) (Add (Number 5) (Number 7)))
