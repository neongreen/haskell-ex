movingAvg r [] = []
movingAvg r list@(x:xs) =
  let left  = take r list
      right = tail list
  in firstHalf left ++ secondHalf r right

firstHalf [] = []
firstHalf list@(x:xs) =
  firstHalf (init list) ++ [(sum list) / fromIntegral (length list)]

secondHalf a list@(x:xs)
  | length list < a = []
  | otherwise       = [sum(take a list) / fromIntegral a] ++ secondHalf a (tail list)
