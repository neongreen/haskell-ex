module LogicBrute where

import Data.List

pairs :: [(Int, Int)]
pairs = [(a, b) | a <- [2..99], b <- [2..99], a >= b]

unique :: [a] -> Bool
unique [_] = True
unique _ = False

-- P knows A*B
mrP :: [Int]
mrP = map (uncurry (*)) pairs

-- S knows A+B
mrS :: [Int]
mrS = map (uncurry (+)) pairs


-- P: I don't know the numbers.
fact1 = not . unique $ mrP

-- S: I knew you didn't know. I don't know either.
fact2 = (not . all unique) $ (group . sort . filter (`elem` mrP)) mrS

-- P: Now I know the numbers
fact3 = undefined

-- S: Now I know them too.
fact4 = undefined
