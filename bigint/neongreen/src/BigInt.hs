module BigInt where


import Data.List
import Data.Monoid


data Sign = Pos | Neg
  deriving (Eq, Show)

-- | A representation of a natural (non-negative) number:
-- 0 is @[]@, 123 is @[3,2,1]@.
type Nat = [Int]

data BigInt = BigInt {
  -- | Sign (positive or negative). 0 is considered positive.
  sign :: Sign,
  nat  :: Nat }
  deriving (Eq, Show)

instance Ord BigInt where
  compare a b = case (sign a, sign b) of
    (Pos, Pos) -> compareNat (nat a) (nat b)
    (Neg, Neg) -> compareNat (nat b) (nat a)
    (Neg, Pos) -> LT
    (Pos, Neg) -> GT

compareNat :: Nat -> Nat -> Ordering
compareNat (a:as) (b:bs) = compareNat as bs <> compare a b
compareNat [] [] = EQ
compareNat [] _  = LT
compareNat _  [] = GT

{- |
Bring all digits into the (0,9) interval and remove leading zeroes. Deals both with overflow and underflow:

>>> normalise [12,10,5]
[2,1,6]
>>> normalise [-1,9]
[9,8]
-}
normalise :: Nat -> Nat
normalise = dropWhileEnd (== 0) . go
  where
    go [] = []
    go [x]
      | x < 0     = error "normalise: negative number"
      | x <= 9    = [x]
      | otherwise = x `mod` 10 : go [x `div` 10]
    go (x:y:zs) = x' : go ((y+d):zs)
      where (d, x') = divMod x 10

zipDigits :: (Int -> Int -> Int) -> Nat -> Nat -> Nat
zipDigits f []     []     = []
zipDigits f (x:xs) []     = f x 0 : zipDigits f xs []
zipDigits f []     (y:ys) = f 0 y : zipDigits f [] ys
zipDigits f (x:xs) (y:ys) = f x y : zipDigits f xs ys

addNat :: Nat -> Nat -> Nat
addNat a b = normalise (zipDigits (+) a b)

subNat :: Nat -> Nat -> Nat
subNat a b = normalise (zipDigits (-) a b)

mulNat :: Nat -> Nat -> Nat
mulNat a b = normalise $ foldr (zipDigits (+)) [] partials
  where
    partials = [replicate i 0 ++ map (*x) b | (i, x) <- zip [0..] a]

biToInteger :: BigInt -> Integer
biToInteger (BigInt Pos xs) = natToInteger xs
biToInteger (BigInt Neg xs) = negate (natToInteger xs)

natToInteger :: Nat -> Integer
natToInteger = sum . zipWith (*) (iterate (10*) 1) . map toInteger

integerToNat :: Integer -> Nat
integerToNat 0 = []
integerToNat n = fromInteger m : integerToNat d
  where (d, m) = divMod n 10

instance Num BigInt where
  fromInteger n = BigInt {
    sign = if n >= 0 then Pos else Neg,
    nat  = integerToNat (abs n) }

  negate (BigInt Pos []) = BigInt Pos []
  negate (BigInt Pos xs) = BigInt Neg xs
  negate (BigInt Neg xs) = BigInt Pos xs

  abs bi = bi {sign = Pos}

  signum 0  = 0
  signum bi = bi {nat = [1]}

  (+) a b = case (sign a, sign b) of
    (Pos, Pos) -> BigInt {sign = Pos, nat = addNat (nat a) (nat b)}
    (Neg, Neg) -> BigInt {sign = Neg, nat = addNat (nat a) (nat b)}
    (Pos, Neg)
      | a >= (-b) -> BigInt {sign = Pos, nat = subNat (nat a) (nat b)}
      | otherwise -> BigInt {sign = Neg, nat = subNat (nat b) (nat a)}
    (Neg, Pos) -> b + a  -- just use the (Pos, Neg) case

  (*) a b = BigInt {
    sign = if a == 0 || b == 0 || sign a == sign b then Pos else Neg,
    nat  = mulNat (nat a) (nat b) }
