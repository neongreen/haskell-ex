{-# LANGUAGE ScopedTypeVariables #-}

module BigIntSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import BigInt

main :: IO ()
main = hspec spec

arbitraryPositive :: Gen Nat
arbitraryPositive = do
  digits <- listOf (choose (0,9))
  final <- choose (1,9)
  return (digits ++ [final])

instance Arbitrary BigInt where
  arbitrary = oneof
    [pure (BigInt Pos []),
     BigInt <$> elements [Pos, Neg] <*> arbitraryPositive]

spec :: Spec
spec = do
  describe "big integer test" $ do
    it "3^200 + 2^300" $
      biToInteger (3^200 + 2^300) `shouldBe` 3^200 + 2^300
    it "300!" $
      biToInteger (product (map fromInteger [1..300])) `shouldBe`
        product [1..300]

  describe "properties" $ do
    it "fromInteger . toInteger == id" $
      property $ \x -> fromInteger (biToInteger x) === x
    it "toInteger . fromInteger == id" $
      property $ \x -> biToInteger (fromInteger x) === x
    it "negate . negate == id" $
      property $ \(x :: BigInt) -> negate (negate x) === x
    it "x-x == 0" $
      property $ \(x :: BigInt) -> x-x === 0
    it "x+x == x*2" $
      property $ \(x :: BigInt) -> x+x === x*2
    describe "commutativity" $ do
      it "x+y == y+x" $
        property $ \(x :: BigInt) y -> x+y === y+x
      it "x*y == y*x" $
        property $ \(x :: BigInt) y -> x*y === y*x
    describe "associativity" $ do
      it "x+(y+z) == (x+y)+z" $
        property $ \(x :: BigInt) y z -> (x+y)+z === x+(y+z)
      it "x*(y*z) == (x*y)*z" $
        property $ \(x :: BigInt) y z -> (x*y)*z === x*(y*z)
    describe "identity element" $ do
      it "x+0 == 0+x == x" $
        property $ \(x :: BigInt) -> x+0 === x .&&. 0+x === x
      it "x*1 == 1*x == x" $
        property $ \(x :: BigInt) -> x*1 === x .&&. 1*x === x

  describe "testing against Integer" $ do
    it "Eq"  $ property $ \x y -> (==) x y === liftBI2 (==) x y
    it "Ord" $ property $ \x y -> compare x y === liftBI2 compare x y
    describe "Num" $ do
      it "negate" $ property $ \x -> negate x === viaBI negate x
      it "abs"    $ property $ \x -> abs x === viaBI abs x
      it "signum" $ property $ \x -> signum x === viaBI signum x
      it "(+)"    $ property $ \x y -> (+) x y === viaBI2 (+) x y
      it "(-)"    $ property $ \x y -> (-) x y === viaBI2 (-) x y
      it "(*)"    $ property $ \x y -> (*) x y === viaBI2 (*) x y

liftBI :: (BigInt -> a) -> Integer -> a
liftBI f x = f (fromInteger x)

liftBI2 :: (BigInt -> BigInt -> a) -> Integer -> Integer -> a
liftBI2 f x y = f (fromInteger x) (fromInteger y)

viaBI :: (BigInt -> BigInt) -> Integer -> Integer
viaBI f x = biToInteger $ liftBI f x

viaBI2 :: (BigInt -> BigInt -> BigInt) -> Integer -> Integer -> Integer
viaBI2 f x y = biToInteger $ liftBI2 f x y
