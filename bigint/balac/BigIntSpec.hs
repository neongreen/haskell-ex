module Main where

import Test.Hspec
import Test.QuickCheck
import BigInt
import Data.Function
import Control.Monad

instance Arbitrary BigInt where
    arbitrary = ( fromInteger :: Integer -> BigInt ) <$> arbitrary

main :: IO ()
main = hspec $ do
    describe "Conversion Identity" $ do
        it "Satisfies conversion identity w.r.t Integers." $ property $
            \num -> bigToInteger ( fromInteger num :: BigInt ) == num

    describe "Equality" $ do
        it "Matches Integer equality." $ property $
            \a b -> ( (==) `on` ( fromInteger::Integer->BigInt ) ) a b == ( a == b )

    describe "Ordering" $ do
        it "Matches Integer ordering." $ property $
            \a b -> ( compare `on` ( fromInteger :: Integer -> BigInt ) ) a b == compare a b

    describe "Addition" $ do
        it "Matches Integer addition." $ property $
            \a b -> bigToInteger ( ( (+) `on` fromInteger ) a b ) == a + b

        it "Is commutative." $ property $
            \a b -> ( ( a + b ) :: BigInt ) == ( b + a )

    describe "Sign Handling" $ do
        it "Matches Integer abs." $ property $
            \a -> bigToInteger ( abs ( fromInteger a ) ) == abs a

        it "Matches Integer signum." $ property $
            \a -> bigToInteger ( signum ( fromInteger a ) ) == signum a

        it "Matches Integer negate." $ property $
            \a -> bigToInteger ( negate ( fromInteger a ) ) == negate a

        it "Satisfies identity x = signum(x) * abs(x)." $ property $
            \a -> signum a * abs a == ( a :: BigInt )

        it "Satisfies identity x = negate . negate x." $ property $
            \a -> ( a :: BigInt ) == ( negate . negate ) a 

    describe "Subtraction" $ do
        it "Matches Integer subtraction." $ property $
            \a b -> bigToInteger ( ( (-) `on` fromInteger ) a b ) == a - b

        it "Changes sign on commutation." $ property $
            \a b -> ( ( a - b ) :: BigInt ) == negate ( b - a )

    describe "Multiplication" $ do
        it "Matches Integer multiplication." $ property $
            \a b -> bigToInteger ( ( (*) `on` fromInteger ) a b ) == a * b

        it "Is commutative." $ property $
            \a b -> ( ( a * b ) :: BigInt ) == ( ( b * a ) :: BigInt )