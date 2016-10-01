module Spiral where

import Data.Matrix

data Direction = R | D | U | L

main :: IO ()
main = do
    putStrLn("Please enter a size for the spiral!")
    i <- getInt
    putStrLn $ concatMap (\line -> (concatMap (\x -> if x == 1 then "* " else "  ") line) ++ "\n") $ toLists $ spiral i 

spiral :: Int -> Matrix Int
spiral i = buildSpiral (zero i i) i R (1,1) 0

buildSpiral :: Matrix Int -> Int -> Direction -> (Int, Int) -> Int -> Matrix Int
buildSpiral m i dir curr count | count >=3 = (setElem 1 curr m)
                               | (available m curr) =
    if (atBound dir curr i m)
    then buildSpiral m i (nextDir dir) curr (count + 1)
    else buildSpiral (setElem 1 curr m) i dir (nextPos dir curr) 0
                         | otherwise = m

available :: Matrix Int -> (Int, Int) -> Bool
available m pos = if (m ! pos) == 1 then False else True 

atBound :: Direction -> (Int,Int) -> Int -> Matrix Int -> Bool
atBound R c@(x,y) i m = (atEnd i R c) || ((distToEnd i R c) > 1 && m ! (x,y+2) == 1)
atBound D c@(x,y) i m = (atEnd i D c) || ((distToEnd i D c) > 1 && m ! (x+2,y) == 1)
atBound L c@(x,y) i m = (atEnd i L c) || ((distToEnd i L c) > 1 && m ! (x,y-2) == 1)
atBound U c@(x,y) i m = (atEnd i U c) || ((distToEnd i U c) > 1 && m ! (x-2,y) == 1)

atEnd :: Int -> Direction -> (Int,Int) -> Bool
atEnd i R (x,y) = i - y == 0
atEnd i D (x,y) = i - x == 0
atEnd _ L (x,y) = y == 1
atEnd _ U (x,y) = x == 1

distToEnd :: Int -> Direction -> (Int,Int) -> Int
distToEnd i R (x,y) = i - y
distToEnd i D (x,y) = i - x
distToEnd _ L (x,y) = y - 1
distToEnd _ U (x,y) = x - 1

nextPos :: Direction -> (Int,Int) -> (Int,Int)
nextPos R (x,y) = (x,y+1)
nextPos D (x,y) = (x+1,y)
nextPos L (x,y) = (x,y-1)
nextPos U (x,y) = (x-1,y)

nextDir :: Direction -> Direction
nextDir R = D
nextDir D = L
nextDir U = R
nextDir L = U

getInt :: IO Int
getInt = do
    str <- getLine
    return (read str)