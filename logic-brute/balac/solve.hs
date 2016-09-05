import GHC.Exts
import Data.List

allNums = [2..99] :: [Int]

type Choice= (Int,Int)
type Space = [Choice]

-- Slice of space such that all Choices in them have the same projection along a particular dimension.
-- The common value of the projection is the `fst` element.
type Slice = (Int,Space)

univ :: Space
univ = [ ( x, y ) | x <- allNums, y <- allNums, x >= y ]

isUnique :: Slice -> Bool
isUnique (_,xs) = length xs == 1

isValueIn :: [Int] -> Slice -> Bool
isValueIn xs (value,_) = value `elem` xs

cSum :: Choice -> Int
cSum (x,y) = x + y

cProduct :: Choice -> Int
cProduct (x,y) = x * y

split :: (Choice->Int) -> (Slice->Bool) -> Space -> (Space,Space)
split proj predicate space = ( matches, remainder )
    where
        slices = map (\lst -> ( proj ( head lst ), lst ) ) $ groupWith proj space
        matchingSlices = filter predicate slices
        matches = concatMap snd matchingSlices
        remainder = space \\ matches

( uniqProds, s1 ) = split cProduct isUnique univ
( _, s2 ) = split cSum ( isValueIn $ map cSum uniqProds ) s1
( s3, _ ) = split cProduct isUnique s2
( s4, _ ) = split cSum isUnique s3

solution  = head s4

main = print solution