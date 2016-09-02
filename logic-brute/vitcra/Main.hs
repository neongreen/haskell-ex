module Main where

import           Data.List

type Pair = (Int, Int)

udiv :: Int -> Int -> Int
udiv n m
  | rem n m == 0 = div n m
  | otherwise = 1 + div n m

divisors :: Int -> [Int]
divisors n = filter ((== 0) . rem n) [2 .. div n 2]

factorPairs :: Int -> [Pair]
factorPairs n = map pair sds where
  ds = divisors n
  sds = take (udiv (length ds) 2) ds
  pair x = (div n x, x)

termPairs :: Int -> [Pair]
termPairs n = map pair [2..div n 2] where
  pair x = (n-x, x)

pTs :: [Pair] -> [[Pair]]
pTs = map (\(a, b) -> termPairs $ a+b)

sTp :: [Pair] -> [[Pair]]
sTp = map (\(a, b) -> factorPairs $ a*b)

-- can the diolog even start?
dialog :: [Pair] -> [Pair] -> Bool
dialog fs ts = not . null $ intersect fs ts

-- definitely cannot know
knowNot :: [Pair] -> Bool
knowNot ps = length ps > 1

-- I know you do not know
sKnowKnowNot :: [Pair] -> Bool
sKnowKnowNot ts = length ts == length ns where
  ns = filter knowNot $ sTp ts

pKnows :: [Pair] -> Bool
pKnows fs = 1 == length ns where
  ns = filter sKnowKnowNot $ pTs fs

sKnows :: [Pair] -> Bool
sKnows ts = 1 == length ns where
  ns = filter pKnows $ sTp ts

isSolution :: Pair -> Bool
isSolution (p, s) =
  dialog fs ts
  && knowNot fs
  && knowNot ts
  && sKnowKnowNot ts
  && pKnows fs
  && sKnows ts
  where
    fs = factorPairs p
    ts = termPairs s

main :: IO ()
main = do
  let ((p,s):_) = filter isSolution [(p, s) | p <- [2..99], s <- [2..99]]
      (numbers:_) = factorPairs p `intersect` termPairs s
  putStrLn $ "The numbers are " ++ show numbers
