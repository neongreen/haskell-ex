data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving Show

evalExpr :: Expr -> Int
evalExpr (Number n) = n
evalExpr (Add a b) = evalExpr a + evalExpr b
evalExpr (Sub a b) = evalExpr a - evalExpr b
evalExpr (Mul a b) = evalExpr a * evalExpr b
evalExpr (Div a b) = evalExpr a `div` evalExpr b

prec :: Expr -> Int
prec e = case e of
  Number{} -> 9
  Add{} -> 1; Sub{} -> 1
  Mul{} -> 2; Div{} -> 2

assoc :: Expr -> Bool
assoc e = case e of
  Number{} -> error "assoc: called on Number"
  Add{} -> True; Sub{} -> False
  Mul{} -> True; Div{} -> False

sameOp :: Expr -> Expr -> Bool
sameOp a b = case (a,b) of
  (Number{}, Number{}) -> error "sameOp: called on Number"
  (Add{}, Add{}) -> True
  (Sub{}, Sub{}) -> True
  (Mul{}, Mul{}) -> True
  (Div{}, Div{}) -> True
  _ -> False

showExpr :: Expr -> String
showExpr e = case e of
  Number n -> parens (n<0) (show n)
  Add a b -> showLeft a ++ "+" ++ showRight b
  Sub a b -> showLeft a ++ "-" ++ showRight b
  Mul a b -> showLeft a ++ "*" ++ showRight b
  Div a b -> showLeft a ++ "/" ++ showRight b
  where
    showLeft  x = parens (prec x < prec e) (showExpr x)
    showRight x = parens (prec x < prec e ||
                          prec x == prec e && not (assoc x && sameOp x e))
                    (showExpr x)

parens :: Bool -> String -> String
parens b s = if b then "(" ++ s ++ ")" else s
