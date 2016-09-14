{-
  # Expr

 ## Goal

    * With a data model for arithmatic, write functions to
      print and exprEvaluate expressions.
    * Use `div` for division.
    * Remember: Use parenthesis around negative numbers.
    * Bonus: Only print parenthesis when they are needed. TODO

 ## Example

    > exprShow (Mul (Number 3) (Add (Number 5) (Number 7)))
    "3*(5+7)"

    > exprEval (Mul (Number 3) (Add (Number 5) (Number 7)))
    36
-}

import qualified Text.Printf as Print



main :: IO ()
main = do
  (putStrLn . exprShow) example
  (print . exprEval) example

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



exprShow :: Expr -> String
exprShow (Number int) = show int
exprShow ex           = Print.printf "(%s %s %s)"
    (exprShow (left ex))
    (operator ex)
    (exprShow (right ex))
  where
  operator (Mul _ _) = "*"
  operator (Add _ _) = "+"
  operator (Div _ _) = "/"
  operator (Sub _ _) = "-"
  operator _ = ""

  left (Mul ex1 _) = ex1
  left (Add ex1 _) = ex1
  left (Div ex1 _) = ex1
  left (Sub ex1 _) = ex1
  left x           = x

  right (Mul _ ex2) = ex2
  right (Add _ ex2) = ex2
  right (Div _ ex2) = ex2
  right (Sub _ ex2) = ex2
  right x           = x



exprEval :: Expr -> Int
exprEval (Number int)  = int
exprEval (Mul ex1 ex2) = (*) (exprEval ex1) (exprEval ex2)
exprEval (Add ex1 ex2) = (+) (exprEval ex1) (exprEval ex2)
exprEval (Div ex1 ex2) = div (exprEval ex1) (exprEval ex2)
exprEval (Sub ex1 ex2) = (-) (exprEval ex1) (exprEval ex2)
