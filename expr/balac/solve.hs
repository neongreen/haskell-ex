
data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
    deriving( Show )

sampleExpr = Mul (Number 3) (Add (Number 5) (Number 7))


evalExpr :: Expr -> Int
evalExpr ( Number n )   = n
evalExpr ( Add l r )    = ( evalExpr l ) + ( evalExpr r )
evalExpr ( Sub l r )    = ( evalExpr l ) - ( evalExpr r )
evalExpr ( Mul l r )    = ( evalExpr l ) * ( evalExpr r )
evalExpr ( Div l r )    = ( evalExpr l ) `div` ( evalExpr r )  


showExpr :: Expr -> String
showExpr ( Number n )   = show n
showExpr ( Add l r )    = concat [ showExpr l, "+", showExpr r ]
showExpr ( Sub l r )    = concat [ showExpr l, "-", showExpr r ]

showExpr ( Mul ( Number l ) ( Number r ) )  = concat [ show l, "*", show r ]
showExpr ( Mul ( Number l ) r )             = concat [ show l, "*(", showExpr r, ")" ]
showExpr ( Mul l ( Number r ) )             = concat [ "(", showExpr l, ")*", show r ]
showExpr ( Mul l r )                        = concat [ "(", showExpr l, ")*(", showExpr r, ")" ]

showExpr ( Div ( Number l ) ( Number r ) )  = concat [ show l, "/", show r ]
showExpr ( Div ( Number l) r )              = concat [ show l, "/(", showExpr r, ")" ]
showExpr ( Div l ( Number r ) )             = concat [ "(", showExpr l, ")/", show r ]
showExpr ( Div l r )                        = concat [ "(", showExpr l, ")/(", showExpr r, ")" ]

main = do
    print sampleExpr
    print $ evalExpr sampleExpr
    print $ showExpr sampleExpr