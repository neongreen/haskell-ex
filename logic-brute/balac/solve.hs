import Data.List
import qualified Data.Set as S
import Data.Function
import GHC.Exts

add (x,y) = x + y
mul (x,y) = x * y

allNums = [2..99] :: [Int]

allPairs = [ (x,y) | x <- allNums, y <- allNums, x < y ]

allPairsSet = S.fromList allPairs

groupedProdPairs = groupWith mul allPairs
nonUniqProdPairs = filter (\x -> length x > 1) groupedProdPairs
pStmt1 = S.fromList $ concat $ nonUniqProdPairs

uniqProdPairs = S.difference allPairsSet pStmt1

groupedSumPairs = groupWith add allPairs

sumPairs :: Int -> [(Int,Int)]
sumPairs num = map (\x -> (x, num - x)) [2 .. num `div` 2]

hasUniqProd :: Int -> Bool
hasUniqProd num = any (\x -> S.member x uniqProdPairs ) $ sumPairs num

sStmt2 = S.fromList $ concat $ filter (not . hasUniqProd . add . head) groupedSumPairs
sStmt2Sums = S.map add sStmt2

pStmt2 = map ( filter (\(x,y) -> S.member (x+y) sStmt2Sums ) ) nonUniqProdPairs
pStmt3 = S.fromList $ concat $ filter ( \x -> length x == 1 ) pStmt2

stmt3Prods = S.map mul pStmt3

sStmt3 = S.filter (\(x,y) -> S.member (x*y) stmt3Prods) sStmt2
groupedSt3Pairs = groupWith add $ S.elems sStmt3
pair = head $ head $ filter (\x -> length x == 1) groupedSt3Pairs

main = do
    print pair