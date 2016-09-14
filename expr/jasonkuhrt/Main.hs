{-
  # Expr

 ## Goal

    * With a data model for arithmatic, write functions to
      print and evaluate expressions.
    * Use `div` for division.
    * Bonus: Only print parenthesis when they are needed.
    * Remember: Use parenthesis around negative numbers.

 ## Example

    > showExpr (Mul (Number 3) (Add (Number 5) (Number 7)))
    "3*(5+7)"

    > evalExpr (Mul (Number 3) (Add (Number 5) (Number 7)))
    36
-}

main :: IO ()
main = undefined

data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
