worlds = [(a,b) | a <- [2..99], b <- [2..99], a>=b]

ambiguous (x:y:_) = True
ambiguous xs      = False

unambiguous [x] = True
unambiguous xs  = False

pWorlds p = [(a,b) | a <- [2..min p 99], let b = div p a, a>=b, b>=2, a*b==p]
sWorlds s = [(a,b) | a <- [2..s], let b = s-a, a>=b, b>=2]

line1 (a,b) = ambiguous (pWorlds (a*b))
line2 (a,b) = ambiguous (sWorlds (a+b)) && all line1 (sWorlds (a+b))
line3 (a,b) = unambiguous (filter line2 (pWorlds (a*b)))
line4 (a,b) = unambiguous (filter line3 (sWorlds (a+b)))

main = print $ filter (\x -> all ($x) [line1,line2,line3,line4]) worlds
