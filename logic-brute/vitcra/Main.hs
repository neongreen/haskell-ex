module Main where

import           Data.List

type Pair = (Int, Int)

factorPairs :: Int -> [Pair]
factorPairs n = [(a,b) | a <- [2..div n 2], (b, 0) <- [divMod n a], a >= b]

termPairs :: Int -> [Pair]
termPairs n = [(n-b, b) | b <- [2..div n 2]]

pTs :: [Pair] -> [[Pair]]
pTs = map (\(a, b) -> termPairs $ a+b)

sTp :: [Pair] -> [[Pair]]
sTp = map (\(a, b) -> factorPairs $ a*b)

-- definitely cannot know
knowNot :: [Pair] -> Bool
knowNot ps = length ps > 1

-- I know you do not know
sKnowKnowNot :: [Pair] -> Bool
sKnowKnowNot ts = all knowNot $ sTp ts

pKnows :: [Pair] -> Bool
pKnows fs = 1 == length ns where
  ns = filter sKnowKnowNot $ pTs fs

sKnows :: [Pair] -> Bool
sKnows ts = 1 == length ns where
  ns = filter pKnows $ sTp ts

isSolution :: Pair -> Bool
isSolution (a, b) =
  and [knowNot fs
  , knowNot ts
  , sKnowKnowNot ts
  , pKnows fs
  , sKnows ts]
  where
    fs = factorPairs $ a*b
    ts = termPairs $ a+b

main :: IO ()
main = do
  let (numbers:_) = filter isSolution [(a, b) | a <- [2..99], b <- [2..99], a >= b]
  putStrLn $ "The numbers are " ++ show numbers
