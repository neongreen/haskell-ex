{-
  # Expr

 ## Goal

    * With a data model for arithmatic, write functions to
      print and evaluate expressions.
    * Use `div` for division.
    * Remember: Use parenthesis around negative numbers.
    * Bonus: Only print parenthesis when they are needed. TODO

 ## Example

    > show (Mul (Number 3) (Add (Number 5) (Number 7)))
    "3*(5+7)"

    > evalExpr (Mul (Number 3) (Add (Number 5) (Number 7)))
    36
-}

import qualified Text.Printf as Print



main :: IO ()
main = do
  print example
  print (eval example)

example :: Expr
example =
  Mul (Number 3)
       (Add (Number 5)
            (Div
              (Sub (Number 7)
                   (Number 1))
              (Number 2)))



data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

instance Show Expr where
  show (Number int)  = show int
  show (Mul ex1 ex2) = Print.printf "(%s * %s)" (show ex1) (show ex2)
  show (Add ex1 ex2) = Print.printf "(%s + %s)" (show ex1) (show ex2)
  show (Div ex1 ex2) = Print.printf "(%s / %s)" (show ex1) (show ex2)
  show (Sub ex1 ex2) = Print.printf "(%s - %s)" (show ex1) (show ex2)

eval :: Expr -> Int
eval (Number int)  =      int
eval (Mul ex1 ex2) =      (*) (eval ex1) (eval ex2)
eval (Add ex1 ex2) =      (+) (eval ex1) (eval ex2)
eval (Div ex1 ex2) =      div (eval ex1) (eval ex2)
eval (Sub ex1 ex2) = subtract (eval ex2) (eval ex1) -- Yes, arguments flipped
