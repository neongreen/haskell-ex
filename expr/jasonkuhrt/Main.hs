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
  print "Should have parens"
  putStrLn . exprShow $ Sub (Number 2) (Sub (Number 2) (Number 1))
  putStrLn . exprShow $ Div (Number 2) (Sub (Number 2) (Number 1))
  putStrLn . exprShow $ Div (Sub (Number 2) (Number 1)) (Number 2)
  putStrLn . exprShow $ example
  print . eval $ example
  print "Should not have any parens"
  putStrLn . exprShow $ Sub (Sub (Number 2) (Number 2)) (Number 1)
  putStrLn . exprShow $ Sub (Number 2) (Div (Number 2) (Number 1))
  putStrLn . exprShow $ Sub (Div (Number 2) (Number 1)) (Number 2)

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

eval :: Expr -> Int
eval (Number int)  = int
eval (Mul ex1 ex2) = (*) (eval ex1) (eval ex2)
eval (Add ex1 ex2) = (+) (eval ex1) (eval ex2)
eval (Div ex1 ex2) = div (eval ex1) (eval ex2)
eval (Sub ex1 ex2) = (-) (eval ex2) (eval ex1) -- Yes, arguments flipped



{- Considering need-only parens

There are two factors influencing the evaluation order of an expression.

## Operator Precedence

Operator precedence assigns different priority to different groups of operators. The following is the abbreviated hierarchy, suitable for our needs:

        ^
       * +
       / -


For example in the following some parens are _required_ for eval order, while others are instead optional.

    Required          Optional
    --------          --------
    (2 + 2) * 2       2 + (2 * 2)  ==  2 + 2 * 2
    (4 - 4) / 2       4 - (4 / 2)  ==  4 - 4 / 2

## Associativity

Operators of equal precedence are evaluated in order of associativity: left-hand-side (lhs) or right-hand side (rhs). Usually operators are lhs; "^*+/-" all are. Consequently when a computation using these operators needs right-before-left then parens are required, but conversely in left-before-right they are optional.

    Required          Optional
    --------          --------
    2 - (2 - 1)       (2 - 2) - 1  ==  2 - 2 - 1
    2 / (4 * 2)       (2 / 4) * 2  ==  2 / 4 * 2

There are two common reasons to use parens: Firstly, for correctness when the operator precedence or associativity does not conform to the computation's needs; Secondly, for readability, when otherwise remembering the hierarchy/associativity (plus mapping it onto the expression) could be mentally taxing, thus error prone.

With this knowledge logic of when to use parens is as follows:

    if exp op is _lower_ precedence than parent-exp op
      REQUIRED
    if exp is RHS &&
      if exp op is _same_ as parent-exp op &&
        if exp op is _not_ associtive
          REQUIRED
    otherwise
      OPTIONAL
-}

lazyParens :: Int -> Expr -> Expr -> String -> String
lazyParens side parent c result
  | (side == 1 && isSameOp parent c && (not . isAssocOp) c ) ||
    isLowerPrecedence c parent =
    Print.printf "(%s)" result
  | otherwise = result

exprShow :: Expr -> String
exprShow expr =
  Print.printf "%s %s %s"
  (go 0 expr (left expr))
  (exprOpShow expr)
  (go 1 expr (right expr))
  where
  go :: Int -> Expr -> Expr -> String
  go _ _ (Number n) = show n
  go 1 parent e =
    lazyParens 1 parent e $ exprShow e
  go 0 parent e =
    lazyParens 0 parent e $ exprShow e
  go _ _ _ = undefined

exprOpShow :: Expr -> String
exprOpShow (Mul _ _)  = "*"
exprOpShow (Div _ _)  = "/"
exprOpShow (Sub _ _)  = "-"
exprOpShow (Add _ _)  = "+"
exprOpShow (Number _) = ""

left :: Expr -> Expr
left (Mul x _) = x
left (Div x _) = x
left (Sub x _) = x
left (Add x _) = x
left (Number x) = Number x

right :: Expr -> Expr
right (Mul _ x) = x
right (Div _ x) = x
right (Sub _ x) = x
right (Add _ x) = x
right (Number x) = Number x

isLowerPrecedence :: Expr -> Expr -> Bool
isLowerPrecedence (Mul _ _) (Add _ _) = False
isLowerPrecedence (Mul _ _) (Sub _ _) = False
isLowerPrecedence (Div _ _) (Add _ _) = False
isLowerPrecedence (Div _ _) (Sub _ _) = False
isLowerPrecedence (Div _ _) (Div _ _) = False
isLowerPrecedence (Mul _ _) (Mul _ _) = False
isLowerPrecedence (Add _ _) (Add _ _) = False
isLowerPrecedence (Sub _ _) (Sub _ _) = False
isLowerPrecedence _         _         = True

isAssocOp :: Expr -> Bool
isAssocOp (Mul _ _) = True
isAssocOp (Add _ _) = True
isAssocOp _         = False

isSameOp :: Expr -> Expr -> Bool
isSameOp (Mul _ _) (Mul _ _) = True
isSameOp (Div _ _) (Div _ _) = True
isSameOp (Add _ _) (Add _ _) = True
isSameOp (Sub _ _) (Sub _ _) = True
isSameOp _         _         = False
