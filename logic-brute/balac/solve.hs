import GHC.Exts
import Data.List

data Choice = Choice { a :: Int, b :: Int, cProd :: Int, cSum :: Int }
                deriving( Show, Eq )

makeChoice :: Int -> Int -> Choice
makeChoice a b = Choice a b (a*b) (a+b)

allNums = [2..99] :: [Int]

type Space = [Choice]

univ :: Space
univ = [ makeChoice x y | x <- allNums, y <- allNums, x >= y ]

type Slice = (Int,Space)

type SlicePredicate = Slice -> Bool

isUniq :: SlicePredicate
isUniq (_,xs) = length xs == 1

isValue :: Int -> SlicePredicate
isValue x (value,_) = x == value

isValueIn :: [Int] -> SlicePredicate
isValueIn xs (value,_) = value `elem` nub xs

data Predicate = ProdPredicate SlicePredicate | SumPredicate SlicePredicate

split :: Predicate -> Space -> ( Space, Space )
split ( ProdPredicate slicePredicate ) = split' cProd slicePredicate
split ( SumPredicate slicePredicate ) = split' cSum slicePredicate

split' :: (Choice->Int) -> SlicePredicate -> Space -> (Space,Space)
split' proj predicate space = ( matches, remainder )
    where
        slices = map (\lst -> ( proj ( head lst ), lst ) ) $ groupWith proj space
        matchingSlices = filter predicate slices
        matches = concatMap snd matchingSlices
        remainder = space \\ matches


( uniqProds, s1 ) = split ( ProdPredicate isUniq ) univ
( _, s2 ) = split ( SumPredicate ( isValueIn $ map cSum uniqProds ) ) s1
( s3, _ ) = split ( ProdPredicate isUniq ) s2
( s4, _ ) = split ( SumPredicate isUniq ) s3

solution  = head s4

main = print ( a solution, b solution )