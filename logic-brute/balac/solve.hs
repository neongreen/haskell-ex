import Data.List
import qualified Data.Set as S
import Data.Function

allNums = [2..99] :: [Int]

canon (x,y) = if x < y then (x,y) else (y,x)

allPairs = nub [ canon (x,y) | x <- allNums, y <- allNums ]

allPairsSet = S.fromList allPairs

sortedProdPairs = sortBy (\(a,b) (c,d) -> compare (a*b) (c*d)) allPairs
groupedProdPairs = groupBy (\(a,b) (c,d) -> (a*b) == (c*d)) sortedProdPairs
nonUniqProdPairs = filter (\x -> length x > 1) groupedProdPairs
pStmt1 = S.fromList $ concat $ nonUniqProdPairs

uniqProdPairs = S.difference allPairsSet pStmt1

sortedSumPairs = sortBy (\(a,b) (c,d) -> compare (a+b) (c+d)) allPairs
groupedSumPairs = groupBy (\(a,b) (c,d) -> (a+b) == (c+d)) sortedSumPairs

sumPairs :: Int -> [(Int,Int)]
sumPairs num = map (\x -> (x, num - x)) [2 .. num `div` 2]

hasUniqProd :: Int -> Bool
hasUniqProd num = any (\x -> S.member x uniqProdPairs ) $ sumPairs num

sStmt2 = S.fromList $ concat $ filter (\((x,y):_) -> not $ hasUniqProd (x+y)) groupedSumPairs
sStmt2Sums = S.map (\(x,y) -> x+y) sStmt2

pStmt2 = map ( filter (\(x,y) -> S.member (x+y) sStmt2Sums ) ) nonUniqProdPairs
pStmt3 = S.fromList $ concat $ filter ( \x -> length x == 1 ) pStmt2

stmt3Rej = S.difference sStmt2 pStmt3
stmt3RejProds = S.map (\(x,y) -> x*y) stmt3Rej
stmt3Prods = S.map( \(x,y) -> x*y ) pStmt3

sStmt3 = S.filter (\(x,y) -> not $ S.member (x*y) stmt3RejProds) sStmt2
sortedSt3Pairs = sortBy (\(a,b) (c,d) -> compare (a+b) (c+d)) $ S.elems sStmt3
groupedSt3Pairs = groupBy (\(a,b) (c,d) -> (a+b) == (c+d)) sortedSt3Pairs
pair = head $ head $ filter (\x -> length x == 1) groupedSt3Pairs

main = do
    print pair