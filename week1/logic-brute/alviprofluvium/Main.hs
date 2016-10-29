module LogicBrute where

import Data.List

type World = (Int, Int)

pairs :: [World]
pairs = [(a, b) | a <- [2..99], b <- [2..99], a >= b]

unique :: [a] -> Bool
unique [_] = True
unique _ = False

-- P knows A*B
pWorlds :: Int -> [World]
pWorlds x = [(a, b) | (a, b) <- pairs, a*b == x]

-- S knows A+B
sWorlds :: Int -> [World]
sWorlds x = [(a, b) | (a, b) <- pairs, a+b == x]


-- P: I don't know the numbers.
fact1 :: World -> Bool
fact1 (a, b) = (not . unique . pWorlds) (a*b)

-- S: I knew you didn't know. I don't know either.
fact2 :: World -> Bool
fact2 (a, b) = notUnique (a+b) && notUnique (a*b)
  where
    notUnique = not . unique . sWorlds

-- P: Now I know the numbers
fact3 :: World -> Bool
fact3 (a, b) = undefined

-- S: Now I know them too.
fact4 = undefined
