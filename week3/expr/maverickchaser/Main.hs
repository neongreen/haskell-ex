data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

data Operation = DoAdd | DoSub | DoMul | DoDiv | DoNothing deriving (Eq, Show)
lastOp :: Expr -> Operation
lastOp (Number a) = DoNothing
lastOp (Add a b) = DoAdd
lastOp (Sub a b) = DoSub
lastOp (Mul a b) = DoMul
lastOp (Div a b) = DoDiv

enclose :: String -> String
enclose a = "(" ++ a ++ ")"

encloseIfNeed a
             | lastOp a == DoAdd || lastOp a == DoSub = enclose
             | otherwise = id

showExpr :: Expr -> String
showExpr (Number a) = show a
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Sub a b) = showExpr a ++ "-" ++ showExpr b
showExpr (Mul a b) = let showA = encloseIfNeed a $ showExpr a
                         showB = encloseIfNeed b $ showExpr b
                     in
                         showA ++ "*" ++ showB
showExpr (Div a b) = let showA = encloseIfNeed a $ showExpr a
                         showB = (if lastOp b == DoNothing then id else enclose) $ showExpr b
                     in
                         showA ++ "/" ++ showB

evalExpr :: Expr -> Int
evalExpr (Number a) = a
evalExpr (Add a b)  = evalExpr a + evalExpr b
evalExpr (Sub a b)  = evalExpr a - evalExpr b
evalExpr (Mul a b)  = evalExpr a * evalExpr b
evalExpr (Div a b)  = evalExpr a `div` evalExpr b

