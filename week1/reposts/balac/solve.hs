import System.Random
import System.IO
import Control.Monad
import Data.Function

kNumExistingPosts = 1000000 :: Int
kNumPosts = [ 10, 100, 1000 ] :: [Int]
kNumWinners = 7 :: Int
kNumTrials = 10000 :: Int

getNonMember :: [Int] -> Int -> IO Int
getNonMember lst range = head . filter ( `notElem` lst ) . randomRs ( 1, range ) <$> newStdGen

pickNums :: Int -> Int -> IO [Int]
pickNums count range = foldM (\x y -> liftM2 (:) ( getNonMember x range ) $ return x ) [] [1..count]

isWin :: Int -> Int -> Int -> IO Bool
isWin numExistingPosts numPosts numWinners =  do
    let totalPosts = numExistingPosts + numPosts
    drawnNums <- pickNums numPosts totalPosts
    return $ any ( <= numWinners ) drawnNums

winProb :: Int -> Int -> Int -> IO Float
winProb numExistingPosts numPosts numTrials = do
    results <- replicateM numTrials $ isWin numExistingPosts numPosts kNumWinners
    return $ ( (/) `on` fromIntegral ) ( length $ filter id results ) numTrials

main = do
    results <- mapM (\x -> winProb kNumExistingPosts x kNumTrials ) kNumPosts
    print results