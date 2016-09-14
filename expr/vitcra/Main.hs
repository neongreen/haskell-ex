module Main where

import           Data.Function (on)

data Expr
  = Number Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

evalExpr :: Expr -> Int
evalExpr e = case e of
  Number n -> n
  Add e1 e2 -> on (+) evalExpr e1 e2
  Sub e1 e2 -> on (-) evalExpr e1 e2
  Mul e1 e2 -> on (*) evalExpr e1 e2
  Div e1 e2 -> on div evalExpr e1 e2


isAddition :: Expr -> Bool
isAddition e = case e of
  Add _ _ -> True
  Sub _ _ -> True
  _ -> False

isMultiplication :: Expr -> Bool
isMultiplication e = case e of
  Mul _ _ -> True
  Div _ _ -> True
  _ -> False


showExpr :: Expr -> String
showExpr e = case e of
  Number n -> if n < 0 then '(' : show n ++ ")" else show n
  Add e1 e2 -> showExpr e1 ++ '+' : showExpr e2
  Sub e1 e2 -> showExpr e1 ++ '-' : wrapAdd e2
  Mul e1 e2 -> wrapAdd e1 ++ '*' : wrapAdd e2
  Div e1 e2 -> wrapAdd e1 ++ " div " ++ wrapDiv e2
  where
    wrap e = '(' : showExpr e ++ ")"
    wrapAdd e = if isAddition e then wrap e else showExpr e
    wrapDiv e = if isAddition e || isMultiplication e
      then wrap e else showExpr e

-- | Turn +- into - and -- into +.
showExprPro :: Expr -> String
showExprPro e = str where
  (minus, str) = helper e
  helper e = case e of
    Number n -> (n < 0, show n)
    Add e1 e2 -> (minus1, str1 ++ if minus2 then str2 else '+' : str2)
      where
        (minus1, str1) = helper e1
        (minus2, str2) = helper e2
    Sub e1 e2 -> (minus1, str1 ++ if minus2 then '+':tail str2 else '-' : str2)
      where
        (minus1, str1) = helper e1
        (minus2, tmp2) = helper e2
        str2 = if isAddition e2 then wrap tmp2 else tmp2
    Mul e1 e2 -> (minus1 && (not . isAddition) e1, str1 ++ '*' : str2)
      where
        (minus1, tmp1) = helper e1
        (minus2, tmp2) = helper e2
        str1 = if isAddition e1 then wrap tmp1 else tmp1
        str2 = if minus2 || isAddition e2 then wrap tmp2 else tmp2
    Div e1 e2 -> (minus1 && (not . isAddition) e1, str1 ++ " div " ++ str2)
      where
        (minus1, tmp1) = helper e1
        (minus2, tmp2) = helper e2
        str1 = if isAddition e1 then wrap tmp1 else tmp1
        str2 = if minus2 || isAddition e2 || isMultiplication e2
          then wrap tmp2 else tmp2
    where
      wrap s = '(' : s ++ ")"

main :: IO ()
main = undefined
