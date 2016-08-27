import System.Random
import System.IO
import Control.Monad

kNumExistingPosts = 1000000 :: Int
kNumPosts = [ 10, 100, 1000 ] :: [Int]
kNumWinners = 7 :: Int
kNumTrials = 10000 :: Int

getNonMember :: [Int] -> Int -> IO Int
getNonMember lst range = do
    n <- getStdRandom( randomR( 1, range ) )
    if n `elem` lst
        then getNonMember lst range
        else return n

pickNums :: Int -> Int -> IO [Int]
pickNums 0 _ = return []
pickNums count range = do
    rest <- pickNums ( count - 1 ) range
    newVal <- getNonMember rest range
    return ( newVal : rest )

isWin :: Int -> Int -> Int -> IO Bool
isWin numExistingPosts numPosts numWinners =  do
    let totalPosts = numExistingPosts + numPosts
    drawnNums <- pickNums numPosts totalPosts
    return $ any id $ map ( <= numWinners ) drawnNums

winProb :: Int -> Int -> Int -> IO Float
winProb numExistingPosts numPosts numTrials = do
    results <- sequence $ take numTrials $ repeat ( isWin numExistingPosts numPosts kNumWinners )
    return $ fromIntegral( length ( filter id results ) ) / ( fromIntegral numTrials )

main = do
    results <- sequence $ map (\x -> winProb kNumExistingPosts x kNumTrials ) kNumPosts
    print results