import Data.List (nub)

import Data.Map (Map)
import qualified Data.Map as Map

check :: Int -> Char -> [Char] -> Int
check _ _ [] = 0        -- Base case: not c in xs
check n c xs = if (head xs)==c then n else check (n+1) c (tail xs)


-- 99 questions @https://wiki.haskell.org/99_questions/1_to_10
-- q5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs++[x]

-- Recursive combine
combine :: String -> String -> [String]
combine [] _ = [[]]
combine _ [] = [[]]
combine s1 s2 = [[i,j] | i<-s1, j<-s2]

-- Returns char or string?
-- let s = createDuplets triplets
createDuplets :: [[Char]] -> [(Char,Char)]
createDuplets [] = []
createDuplets ([a,b,c]:xs) = nub $ (a,b):(b,c):createDuplets (xs)


triplets = ["tup"
           ,"whi"
           ,"tsu"   -- tup -> tsup
           ,"ats"   -- atsup
           ,"hap"   -- hatsup
           ,"tis"   -- hatisup
           ,"whs"    
           ]

s = createDuplets triplets
m1 = Map.fromList s