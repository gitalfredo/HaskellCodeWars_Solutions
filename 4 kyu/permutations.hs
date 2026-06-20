import Data.List (nub, sort)
-- Official (of sort) solution + nub ._.
p :: String -> [String]
p [] = [[]]
p s = [i:j | i <- s, j <- p(deleter i s)]
 
-- Delete char
deleter :: Char -> [Char] -> [Char]
deleter c s = del c s [] where 
    del c [] acc = acc
    del c (x:xs) acc = if x==c then (acc++xs) else (del c xs (x:acc)) -- If match then return what has been accumulated plus the rest
 
-- My Solution - finding permutations by applying product rule then filtering the result 
permutations :: String -> [String] 
permutations s = nub $ filter' [] $ sequence $ replicate (length s) s where
    filter' acc []       = acc
    filter' acc y@(x:xs) = if valueCompare s x then filter' (x:acc) xs else filter' acc xs

count :: Char -> String -> Int
count c s = sum $ map (\e -> if e==c then 1 else 0) s

-- Apply count to all elements in a string
countAll :: String -> [Int]
countAll s = count' s s where
    count' []     _ = []
    count' (x:xs) s = count x s:count' xs s

valueCompare :: String -> String -> Bool
valueCompare s1 s2 = countAll (sort s1) == countAll (sort s2)
 