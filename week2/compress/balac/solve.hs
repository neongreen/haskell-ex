import Data.List
import Test.QuickCheck
import Debug.Trace

kMinMatch = 3

commonPrefixLength :: Eq a => [a] -> [a] -> Int
commonPrefixLength xs ys = length $ takeWhile ( uncurry (==) ) $ zip xs ys

type Match = (Int,Int)

findMatches :: String -> String -> Int -> [ Match ]
findMatches [] _ _ = []
findMatches str1 str2 startOffset =   
    if cpLength < kMinMatch
        then rest
        else ( startOffset, cpLength ) : rest 
    where
        rest = findMatches ( tail str1 ) str2 ( startOffset + 1 )
        cpLength = commonPrefixLength str1 str2

biggestMatch :: String -> String -> Maybe Match
biggestMatch str1 str2 = case findMatches str1 str2 0 of
                            [] -> Nothing
                            xs -> Just ( ( head . reverse ) $ sortOn snd xs )

matchSuffixes :: [String] -> [ (Int,String) ] -> [ (Int, Match) ]
matchSuffixes (prefix:prefixes) ((soffset,suffix):suffixes) = 
    case biggestMatch prefix suffix of
        Nothing ->  matchSuffixes prefixes suffixes
        Just match@( moffset, prefixLength ) -> (soffset, match) : matchSuffixes ( drop ( prefixLength - 1 ) prefixes ) ( drop ( prefixLength - 1 ) suffixes )
matchSuffixes _ _ = []

type Token = Either String Match

toTokens :: String -> Int -> [(Int,Match)] -> [Token]
toTokens [] _ _ = []
toTokens str pos [] = case drop pos str of
                        "" -> []
                        rest  -> [ Left rest ]
toTokens str pos ((offset,m@(_,prefixLength)):matches) = 
    if textLen > 0
        then Left ( take textLen $ drop pos str ) : tokens
        else tokens
    where
        textLen= offset - pos
        tokens = Right m : toTokens str ( offset + prefixLength ) matches

compress :: String -> [Token]
compress "" = []
compress str = toTokens str 0 matches
    where 
        prefixes = tail $ inits str
        suffixes = tail $ zip [0..] ( tails str )
        matches = matchSuffixes prefixes suffixes

decompress :: [Token] -> String
decompress = decompress' ""

decompress' :: String -> [Token] -> String
decompress' str [] = str
decompress' str ( Left x : tokens ) = decompress' ( str ++ x ) tokens
decompress' str ( Right (mpos,mlen) : tokens ) = decompress' ( str ++ take mlen ( drop mpos str ) ) tokens


prop_inverse :: String -> Bool
prop_inverse str = decompress ( compress str ) == str

sampleOut1, sampleOut2, sampleOut3 :: [ Either String (Int,Int) ]

sampleInp1 = "Consider a string. No, consider a different string. Whatever."
sampleOut1 = [ Left "Consider a string. No, c", Right (1,10), Left "different", Right (10,9), Left "Whatever." ]
sampleInp2 = "foo|bar|foobar"
sampleOut2 = [Left "foo|bar|",Right (0,3),Right (4,3)]
sampleInp3 = "foo|foox:foox"
sampleOut3 = [Left "foo|",Right (0,3),Left "x:",Right (4,4)]


main :: IO ()
main = do
    quickCheck $ compress sampleInp1 == sampleOut1
    quickCheck $ compress sampleInp2 == sampleOut2
    quickCheck $ compress sampleInp3 == sampleOut3
    quickCheck prop_inverse

--TODO: Rewrite using State.