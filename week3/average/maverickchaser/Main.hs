average a = sum a / fromIntegral (length a)

-- Method 1


moving k a = premoving (take (k-1) a) 0 1 ++ movingImpl 0
    where premoving  [] _ _ = []
          premoving (x:a) cur index = (x+cur) / fromIntegral index : premoving a (cur+x) (index+1)
          movingImpl index
                     | index+k <= length a = average (take k (drop index a)) : movingImpl $ index+1
                     | otherwise           = []
