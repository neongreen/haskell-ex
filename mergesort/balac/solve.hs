import Data.List
import Data.Char

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort lst = merge ( mergeSort left ) ( mergeSort right )
    where
        ( left, right ) = splitAt ( length lst `div` 2 ) lst
        merge [] xs = xs
        merge xs [] = xs
        merge first@(x:xs) second@(y:ys) = if x < y
                                                then x : merge xs second
                                                else y : merge first ys


pangram = filter ( /= ' ' ) $ map toLower "The quick brown fox jumps over the lazy dog"

main = do
    print pangram
    print ( mergeSort pangram )
    print $ nub ( mergeSort pangram ) == ['a'..'z']