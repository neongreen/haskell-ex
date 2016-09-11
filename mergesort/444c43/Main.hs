splitlist :: [a] -> ([a], [a])
splitlist list = splitAt ((length (list) + 1) `div` 2) list
