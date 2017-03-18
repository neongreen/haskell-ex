module Main where

newtype SRCRange = SRCRange {getRange :: (Int, Int)} deriving (Eq, Show)
newtype PairValue = PairValue {getPair :: (Int, Int)} deriving (Eq, Show)

a :: PairValue -> Int
a = fst.getPair

b :: PairValue -> Int
b = snd.getPair

prdVal :: PairValue -> Int
prdVal pv = a pv * b pv

smVal :: PairValue -> Int
smVal pv = a pv + b pv

rs :: SRCRange -> Int
rs = fst.getRange

re :: SRCRange -> Int
re = snd.getRange

range :: SRCRange -> [Int]
range r = [rs r .. re r]

inR :: SRCRange -> Int -> Bool
inR r v = v >= rs r && v <= re r

maxSum :: SRCRange -> Int
maxSum r = 2 * re r

minSum :: SRCRange -> Int
minSum r = 2 * rs r

maxProd :: SRCRange -> Int
maxProd r = re r * re r

minProd :: SRCRange -> Int
minProd r = rs r * rs r

allPairs :: SRCRange -> [PairValue]
allPairs r = [PairValue (a', b') | a' <- range r, b' <- range r, a' >= b']

getSums :: SRCRange -> Int -> [PairValue]
getSums r sm =  [PairValue (a', sm - a') | a' <- range r, valid a']
                where
                  valid a' = a' <= sm - a' && inR r (sm - a')

getProds :: SRCRange -> Int -> [PairValue]
getProds r prd = [PairValue (a', prd `div` a') | a' <- range r, valid a']
                 where
                   valid a' = dm == 0 && a' <= dr && inR r dr
                                  where
                                    dr = prd `div` a'
                                    dm = prd `mod` a'


isMultiple :: [a] -> Bool
isMultiple reps = length reps >= 2

isSingle :: [a] -> Bool
isSingle reps = length reps == 1

mrPNotKnowsNumbers :: SRCRange -> Int -> Bool
mrPNotKnowsNumbers r = isMultiple . getProds r

mrSNotKnowsNumbers :: SRCRange -> Int -> Bool
mrSNotKnowsNumbers r = isMultiple . getSums r

mrSKnowMrPNotKnowsNumbers :: SRCRange -> Int -> Bool
mrSKnowMrPNotKnowsNumbers r = all (mrPNotKnowsNumbers r.prdVal) . getSums r

mrPKnowsNumbers :: SRCRange -> Int -> Bool
mrPKnowsNumbers r prod =  isSingle $ filter valid sumSubLists
                          where
                            sumSubLists = map (getSums r.smVal) (getProds r prod)
                            valid = all (mrSKnowMrPNotKnowsNumbers r.smVal)

mrSKnowsNumbers :: SRCRange -> Int -> Bool
mrSKnowsNumbers r s = isSingle $ filter valid prodSubLists
                      where
                        prodSubLists = map (getProds r.prdVal) (getSums r s)
                        valid = all (mrPKnowsNumbers r.prdVal)

filterAnswers :: [PairValue -> Bool] -> PairValue -> Bool
filterAnswers fs val = all ($ val) fs

isAnswer :: SRCRange -> PairValue -> Bool
isAnswer r = filterAnswers
  [
    mrSNotKnowsNumbers r.smVal,
    mrPNotKnowsNumbers r.prdVal,
    mrSKnowMrPNotKnowsNumbers r.smVal,
    mrSKnowsNumbers r.smVal,
    mrPKnowsNumbers r.prdVal
  ]


getSolutions :: SRCRange -> [PairValue]
getSolutions r = filter (isAnswer r) (allPairs r)

main :: IO()
main = print $ getSolutions (SRCRange (2,99))
