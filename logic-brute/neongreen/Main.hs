type World = (Int, Int)

worlds :: [World]
worlds = [(a,b) | a <- [2..99], b <- [2..99], a>=b]

-- Faster than “length xs > 1”
ambiguous :: [World] -> Bool
ambiguous (x:y:_) = True
ambiguous xs      = False

unambiguous :: [World] -> Bool
unambiguous [x] = True
unambiguous xs  = False

-- All worlds in which the product is X.
pWorlds :: Int -> [World]
pWorlds p = [(a,b) | (a,b) <- worlds, a*b == p]

-- All worlds in which the sum is X.
sWorlds :: Int -> [World]
sWorlds s = [(a,b) | (a,b) <- worlds, a+b == s]

-- | Line 1: P doesn't know the numbers.
line1 :: World -> Bool
line1 (a,b) =
  -- If actual numbers are (a,b), then with what P knows it's impossible to
  -- determine the numbers unambiguously
  ambiguous (pWorlds p)
  where
    p = a*b

-- | Line 2: S knows that P doesn't know the numbers, and S doesn't know the
-- numbers either.
line2 :: World -> Bool
line2 (a,b) =
  -- S doesn't know the numbers
  ambiguous (sWorlds s)  &&
  -- S knows that P doesn't know the numbers, i.e. line 1 is true for *all*
  -- possible worlds
  all line1 (sWorlds s)
  where
    s = a+b

-- | Line 3: now that P knows information from line 2, P knows the numbers.
line3 :: World -> Bool
line3 (a,b) =
  -- There's only one world that is consistent with what P knows (i.e. the
  -- product) and in which line2 holds
  unambiguous (filter line2 (pWorlds p))
  where
    p = a*b

-- | Line 4: now that S knows information from line 3, S knows the numbers.
line4 :: World -> Bool
line4 (a,b) =
  -- There's only one world that is consistent with what S knows (i.e. the
  -- sum) and in which line3 holds
  unambiguous (filter line3 (sWorlds s))
  where
    s = a+b

main = print $ filter (\x -> all ($x) [line1,line2,line3,line4]) worlds
