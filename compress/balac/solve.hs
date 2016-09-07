import Data.List
import Test.QuickCheck
import Debug.Trace

kMinMatch = 3

commonPrefixLength :: Eq a => [a] -> [a] -> Int
commonPrefixLength xs ys = length $ takeWhile ( uncurry (==) ) $ zip xs ys

type Match = (Int,Int)

findMatch :: String -> String -> Int -> Maybe Match
findMatch [] _ _ = Nothing
findMatch str1 str2 startOffset =   
    if cpLength < kMinMatch
        then findMatch ( tail str1 ) str2 ( startOffset + 1 )
        else Just ( startOffset, cpLength ) 
    where
        cpLength = commonPrefixLength str1 str2


matchSuffixes :: [String] -> [ (Int,String) ] -> [ (Int, Match) ]
matchSuffixes (prefix:prefixes) ((soffset,suffix):suffixes) = 
    case findMatch prefix suffix 0 of
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

sampleStr1 = "Consider a string. No, consider a different string. Whatever."
sampleStr2 = "foo|bar|foobar"

main = quickCheck prop_inverse

--TODO: Rewrite using State.