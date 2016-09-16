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
  putStrLn "\n# Required\n"
  putStrLn . visualize $ Div (Number 10) (Number (-1))
  putStrLn . visualize $ Sub (Number 2) (Sub (Number 2) (Number 1))
  putStrLn . visualize $ Div (Number 2) (Sub (Number 2) (Number 1))
  putStrLn . visualize $ Div (Sub (Number 2) (Number 1)) (Number 2)
  putStrLn $ visualize example ++ " == " ++ (show . eval) example
  putStrLn "\n# Optional\n"
  putStrLn . visualize $ Sub (Sub (Number 2) (Number 2)) (Number 1)
  putStrLn . visualize $ Sub (Number 2) (Div (Number 2) (Number 1))
  putStrLn . visualize $ Sub (Div (Number 2) (Number 1)) (Number 2)

example :: Expr
example =
  Mul (Number (-3))
      (Add (Number 5)
           (Div (Sub (Number 7)
                     (Number 1))
                 (Number 2)))

data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr



-- Evaluate An Expression --

eval :: Expr -> Int
eval (Number int)  = int
eval (Mul ex1 ex2) = (*) (eval ex1) (eval ex2)
eval (Add ex1 ex2) = (+) (eval ex1) (eval ex2)
eval (Div ex1 ex2) = div (eval ex1) (eval ex2)
eval (Sub ex1 ex2) = (-) (eval ex1) (eval ex2)


-- Visualize An Expression --

{- Considering need-only parens

There are two factors influencing the evaluation order of an expression.

## Operator opPrecedence

Operator opPrecedence assigns different priority to different groups of operators. The following is the abbreviated hierarchy, suitable for our needs:

        ^
       * +
       / -


For example in the following some parens are _required_ for eval order, while others are instead optional.

    Required          Optional
    --------          --------
    (2 + 2) * 2       2 + (2 * 2)  ==  2 + 2 * 2
    (4 - 4) / 2       4 - (4 / 2)  ==  4 - 4 / 2

## Associativity

Operators of equal opPrecedence are evaluated in order of associativity: left-hand-side (lhs) or right-hand side (rhs). Usually operators are lhs; "^*+/-" all are. Consequently when a computation using these operators needs right-before-left then parens are required, but conversely in left-before-right they are optional.

    Required          Optional
    --------          --------
    2 - (2 - 1)       (2 - 2) - 1  ==  2 - 2 - 1
    2 / (4 * 2)       (2 / 4) * 2  ==  2 / 4 * 2

There are two common reasons to use parens: Firstly, for correctness when the operator opPrecedence or associativity does not conform to the computation's needs; Secondly, for readability, when otherwise remembering the hierarchy/associativity (plus mapping it onto the expression) could be mentally taxing, thus error prone.

With this knowledge logic of when to use parens is as follows:

    if exp op is _lower_ opPrecedence than parent-exp op
      REQUIRED
    if exp is RHS &&
      if exp op is _same_ as parent-exp op &&
        if exp op is _not_ associtive
          REQUIRED
    otherwise
      OPTIONAL
-}

visualize :: Expr -> String
visualize (Number n)
  | n < 0     = parenthesize  (show n)
  | otherwise = show n
visualize p          =
  Print.printf "%s %s %s" goLeft (op p) goRight
  where
  goLeft  = ifthen (needsParensLHS p) parenthesize . visualize $ left p
  goRight = ifthen (needsParensRHS p) parenthesize . visualize $ right p

  op (Mul _ _)  = "*"
  op (Div _ _)  = "/"
  op (Sub _ _)  = "-"
  op (Add _ _)  = "+"
  op _          = "" -- NOTE Impossible case. See parent pattern matches.


needsParensRHS :: Expr -> Bool
needsParensRHS p = let c = right p in
  opPrecedence p > opPrecedence c ||
  (isOpSame p c && (not . isOpAssoc) c)

needsParensLHS :: Expr -> Bool
needsParensLHS p =
  opPrecedence p > opPrecedence (left p)


opPrecedence :: Expr -> Int
opPrecedence (Number _) = 100
opPrecedence (Mul _ _)  = 9
opPrecedence (Div _ _)  = 9
opPrecedence (Sub _ _)  = 8
opPrecedence (Add _ _)  = 8


{- | Is the operator of a given expression associative?

Certain operators are associative. An associative operator is one whose evaluation order is insignificant. For example the operator `+` is associative so the expression `1 + 2 + 3` it can be evaluated in either order: `1 + (2 + 3)` or `(1 + 2) + 3`. It is insignificant.

A use-case for this information is when, in the act of visualizing an expression tree, deciding whether certain parts needs parens to be correct. -}
isOpAssoc :: Expr -> Bool
isOpAssoc (Mul _ _) = True
isOpAssoc (Add _ _) = True
isOpAssoc _         = False


{- | Do two given expressions use the same operators? -}
isOpSame :: Expr -> Expr -> Bool
isOpSame (Mul _ _) (Mul _ _) = True
isOpSame (Div _ _) (Div _ _) = True
isOpSame (Add _ _) (Add _ _) = True
isOpSame (Sub _ _) (Sub _ _) = True
isOpSame _         _         = False


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



-- General Helpers --

parenthesize :: String -> String
parenthesize = Print.printf "(%s)"

ifthen :: Bool -> (a -> a) -> a -> a
ifthen True  f val = f val
ifthen False _ val = val
