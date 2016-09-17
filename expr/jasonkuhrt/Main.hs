{- README

--- Expr

 -- Goal

    * With a data model for arithmatic, write functions to
      print and evaluate expressions.
    * Use `div` for division.
    * Remember: Use parenthesis around negative numbers.
    * Bonus: Only print parenthesis when they are needed.
-}

import qualified Text.Printf as Print



main :: IO ()
main = do
  putStrLn "\n# Required\n"
  putStrLn . visualize $ Sub (Number 1) (Add (Number 2) (Number 3))
  putStrLn . visualize $ Add (Number 1) (Sub (Number 2) (Number 3))
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
                 (Number (-2))))

data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr



{- | Evaluate An Expression.

> eval (Mul (Number 3) (Add (Number 5) (Number 7)))
36
-}
eval :: Expr -> Int
eval (Number int) = int
eval (Mul l r)    = (*) (eval l) (eval r)
eval (Add l r)    = (+) (eval l) (eval r)
eval (Div l r)    = div (eval l) (eval r)
eval (Sub l r)    = (-) (eval l) (eval r)


{- | Visualize An Expression

Parenthesis will only be used where necessary.

> visualize (Sub (Number 1) (Add (Number 2) (Number 3)))
"1 - (2 + 3)"

> visualize (Mul (Number 3) (Add (Number 5) (Number 7)))
"3 * (5 + 7)"

> visualize (Mul (Number (-3)) (Add (Number 5) (Div (Sub (Number 7) (Number 1)) (Number 2))))
"(-3) * (5 + (7 - 1) / 2)"



--- Solution Notes

    The hard part is need-only parens. Our solution must model the two
    factors that influence the evaluation order of expressions.

--- Factor 1: Operator Precedence

    Operator precedence assigns different priority to different groups of
    operators. The following is the abbreviated hierarchy, suitable for our
    needs:

          ^
         * +
         / -

    For example in the following some parens are _required_ for eval order,
    while others are instead optional.

        Required          Optional
        --------          --------
        (2 + 2) * 2       2 + (2 * 2)  ==  2 + 2 * 2
        (4 - 4) / 2       4 - (4 / 2)  ==  4 - 4 / 2

--- Factor 2: Associativity

    Operators of equal precedence are evaluated in order of their left or
    right associativity. Usually operators are left-associative
    (^ * + / - all are). When a computation using a left-associative operator
    has a nested right expression or vice-versa (left expression in a
    right-associative operator) then parens will be required, which one exception.

        Required          Optional
        --------          --------
        2 - (2 - 1)       (2 - 2) - 1  ==  2 - 2 - 1
        2 / (4 * 2)       (2 / 4) * 2  ==  2 / 4 * 2

    The exception is fully-associative parent operators wherein parenthesis
    are not required! Consider:

                          1 + (2 - 1)  ==  1 + 2 - 1  ==  2

    The reason for this case might be more clear when subtraction is
    made into explicit addition of negative numbers:

        SAME!
        1+2-1    ->  1 +2 +(-1)
        1+(2-1)  ->  1 +2 +(-1)

    But again with left-associative parent

      DIFFERENT!
      1-(2+1)  ->  1 +(-2) +(-1)
      1-2+1    ->  1 +(-2) +1

--- Conclusion

    With this knowledge, the logic of when to use parens is:

        if: exp op is _lower_ precedence than parent-exp op
          REQUIRED
        if: parent-exp op is LA (_not_ fully associative)
          if: right child-exp
            REQUIRED TODO
        if: parent-exp op is RA (_not_ fully associative)
          if: left child-exp
            REQUIRED TODO
        else
          OPTIONAL

--- Aside

    There are two common reasons to use parentheses: Firstly, for correctness
    when the operator precedence or operator associativity does not conform
    to the computation's needs; Secondly, for readability, when otherwise
    remembering the precedence hierarchy/associativity (plus mapping it onto
    the expression) could be mentally taxing (thus error prone).
-}
visualize :: Expr -> String
visualize (Number n) = ifthen (n < 0) parenthesize . show $ n
visualize p          =
  Print.printf "%s %s %s" goLeft (op p) goRight
  where
  goLeft  = ifthen (needsParensLHS p) parenthesize . visualize $  left p
  goRight = ifthen (needsParensRHS p) parenthesize . visualize $ right p

  op (Mul _ _)  = "*"
  op (Div _ _)  = "/"
  op (Sub _ _)  = "-"
  op (Add _ _)  = "+"
  -- Impossible case. See parent pattern matches.
  op _          = error "No operator for leaves"


-- TODO Correct LA/R RA/L paren requirement. See comments above
needsParensRHS :: Expr -> Bool
needsParensRHS p = let c = right p in
     opPrecedence p  > opPrecedence c
  || opPrecedence p == opPrecedence c && (not . isOpAssoc) p

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

Certain operators are associative. An associative operator is one whose
evaluation order is insignificant. For example the operator `+` is
associative so the expression `1 + 2 + 3` it can be evaluated in either
order: `1 + (2 + 3)` or `(1 + 2) + 3`. It is insignificant.

A use-case for this information is when, in the act of visualizing an
expression tree, deciding whether certain parts needs parens to be correct.
-}
isOpAssoc :: Expr -> Bool
isOpAssoc (Mul _ _) = True
isOpAssoc (Add _ _) = True
isOpAssoc _         = False


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
