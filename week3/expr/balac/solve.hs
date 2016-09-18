
data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
    deriving( Show )

sampleExpr = Mul (Number (-2)) ( Mul (Number 3) (Add (Number 5) (Number 7)) )


evalExpr :: Expr -> Int
evalExpr ( Number n )   = n
evalExpr ( Add l r )    = ( evalExpr l ) + ( evalExpr r )
evalExpr ( Sub l r )    = ( evalExpr l ) - ( evalExpr r )
evalExpr ( Mul l r )    = ( evalExpr l ) * ( evalExpr r )
evalExpr ( Div l r )    = ( evalExpr l ) `div` ( evalExpr r )  


showExpr :: Expr -> String
showExpr ( Number n )   = if n < 0 then concat [ "(", show n , ")" ] else show n
showExpr ( Add l r )    = concat [ showExpr l, "+", showExpr r ]
showExpr ( Sub l r )    = concat [ showExpr l, "-", showExpr r ]

showExpr ( Mul l@( Number _ ) r@( Number _ ) )  = concat [ showExpr l, "*", showExpr r ]
showExpr ( Mul l@( Number _ ) r )               = concat [ showExpr l, "*(", showExpr r, ")" ]
showExpr ( Mul l r@( Number _ ) )               = concat [ "(", showExpr l, ")*", showExpr r ]
showExpr ( Mul l r )                            = concat [ "(", showExpr l, ")*(", showExpr r, ")" ]

showExpr ( Div l@( Number _ ) r@( Number _ ) )  = concat [ showExpr l, "/", showExpr r ]
showExpr ( Div l@( Number _) r )                = concat [ showExpr l, "/(", showExpr r, ")" ]
showExpr ( Div l r@( Number _ ) )               = concat [ "(", showExpr l, ")/", showExpr r ]
showExpr ( Div l r )                            = concat [ "(", showExpr l, ")/(", showExpr r, ")" ]

main = do
    print sampleExpr
    print $ evalExpr sampleExpr
    print $ showExpr sampleExpr