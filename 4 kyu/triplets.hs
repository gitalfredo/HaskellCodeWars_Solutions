import Data.List (sort,sortBy,nub)
import Data.Ord  (comparing)
import Data.Maybe 
 
recoverSecret :: [String] -> String
recoverSecret triplets = iterateDuplets $ createDuplets triplets
 
createDuplets :: [[Char]] -> [(Char,Char)]
createDuplets [] = []
createDuplets ([a,b,c]:xs) = nub $ (a,b):(b,c):createDuplets (xs)

-- Find element without an vertex coming to it 
findFirst :: [(Char,Char)] -> Char 
findFirst s@(x:xs) = if length s > 1 then (findFirst' s) else fst x where
  findFirst' s = [fst i | i<- s, (filter (\e -> (fst i == snd e)) s)==[]] !! 0

-- Applies findFirst on each tuple
iterateDuplets :: [(Char,Char)] -> [Char]
iterateDuplets [] = []
iterateDuplets [x] = [fst x]++[snd x]
iterateDuplets s  = x : iterateDuplets (filter (\e -> fst e /= x) s) where
  x = findFirst s
 
-- Tests variables
secret = "whatisup"
xs = ["tup"
           ,"whi"
           ,"tsu"   -- tup -> tsup
           ,"ats"   -- atsup
           ,"hap"   -- hatsup
           ,"tis"   -- hatisup
           ,"whs"    
           ]


s = createDuplets $ sort xs 
 