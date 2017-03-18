import Control.Monad
import System.Random
import Data.List

numberOfReposters       = 1000000
numberOfFakeAccounts    = 100000
numberOfSimulations     = 1000
numberOfWinners         = 7


generateAccounts :: Int -> Int -> StdGen -> [Int]
generateAccounts nAccounts maxAccId gen =
    take nAccounts . map snd . sort $ zip (randoms gen :: [Int]) [1..maxAccId]

isWinner :: [Int] -> [Int] -> Bool
isWinner fakeAccounts winners = any (`elem` winners) fakeAccounts

simulate = do
    gen <- newStdGen
    let fakeAccounts = generateAccounts numberOfFakeAccounts numberOfReposters gen
    gen <- newStdGen
    let winners      = generateAccounts numberOfWinners numberOfReposters gen
    return (isWinner fakeAccounts winners)

main = do
    gen <- newStdGen
    let fakeAccounts = generateAccounts numberOfFakeAccounts numberOfReposters gen
    gen <- newStdGen
    let winners      = generateAccounts numberOfWinners numberOfReposters gen
    results <- replicateM numberOfSimulations simulate
    let probability = fromIntegral (length $ filter (==True) results) / fromIntegral numberOfSimulations
    putStrLn $ "Probability of winning is " ++ show probability

