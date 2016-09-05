import Data.List
import Test.QuickCheck

kMinMatch = 3

sampleStr = "Consider a string. No, consider a different string. Whatever."

commonPrefixLength :: Eq a => [a] -> [a] -> Int -> Int
commonPrefixLength [] _ count = count
commonPrefixLength _ [] count = count
commonPrefixLength (x:xs) (y:ys) count = if x == y
                                            then commonPrefixLength xs ys ( count + 1 )
                                            else count

type Match = (Int,Int)

findMatch :: String -> String -> Int -> Int -> Maybe Match
findMatch str1 str2 startOffset endOffset
    | startOffset == endOffset = Nothing
    | otherwise = if cpLength < kMinMatch
                    then findMatch ( tail str1 ) str2 ( startOffset + 1 ) endOffset
                    else Just ( startOffset, cpLength ) 
    where
        cpLength = commonPrefixLength str1 str2 0


matchSuffixes :: String -> [ (Int,String) ] -> [ (Int, Match) ]
matchSuffixes [] _ = []
matchSuffixes str [] = []
matchSuffixes str ((soffset,suffix):suffixes) = 
    case findMatch str suffix 0 soffset of
        Nothing ->  matchSuffixes str suffixes
        Just match@( moffset, prefixLength ) -> (soffset, match) : matchSuffixes str ( drop prefixLength suffixes )

type Token = Either String Match

toTokens :: String -> Int -> [(Int,Match)] -> [Token]
toTokens [] _ _ = []
toTokens str pos [] = [ Left $ drop pos str ]
toTokens str pos ((offset,m@(_,prefixLength)):matches) = Left ( take ( offset - pos ) $ drop pos str ) : Right m : toTokens str ( offset + prefixLength ) matches

compress :: String -> [Token]
compress "" = [Left ""]
compress str = toTokens str 0 matches
    where 
        suffixes = tail $ zip [0..] ( tails str )
        matches = matchSuffixes str suffixes

decompress :: [Token] -> String
decompress = decompress' ""

decompress' :: String -> [Token] -> String
decompress' str [] = str
decompress' str ( Left x : tokens) = decompress' ( str ++ x ) tokens
decompress' str ( Right (mpos,mlen) : tokens) = decompress' ( str ++ take mlen ( drop mpos str ) ) tokens


prop_inverse :: String -> Bool
prop_inverse str = decompress ( compress str ) == str

main = quickCheck prop_inverse