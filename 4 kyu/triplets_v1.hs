import Data.List (sort, nub)
{- 
  Topological sort
  1 Find node without dependencies going into it
  2 Iterate through all dependencies and add them to list L (...) 

  Preconditions:
  Assumes each character is a unique Node value (implicitly given)
-}

recoverSecret :: [String] -> String
recoverSecret triplets = undefined

data DAG a = 
  Elem a 
  | List [DAG a] deriving Show
 
-- Purpose: Duples are given DAG relations origin->target 
-- Returns: duples using triplets indices 0,1 and 1,2
-- let s = createDuplets $ sort triplets
createDuplets :: [[Char]] -> [(Char,Char)]
createDuplets [] = []
createDuplets ([a,b,c]:xs) = nub $ (a,b):(b,c):createDuplets (xs)
   
-- let (a,b) = createDictionary triplets
createDictionary :: [[Char]] -> ([Char],[Char])
createDictionary s = ((nub [i | (i,_)<-createDuplets s]), (nub [i | (_,i)<-createDuplets s]))

remover :: [a] -> Int -> [a]
remover s n = take n s ++ drop (n+1) s

-- Extract all characters in the secret word
-- length $ getUnique triplets
-- let letters = getUnique triplets
getUnique :: [[Char]] -> [Char]
getUnique s = nub $ filter (/=' ') $ unwords s

-- Finds incoming edge count for specific Node value
-- incoming :: DAG -> a -> DAG
-- incoming Empty _ = Empty
-- incoming (Node a (s:xs)) c = if c==a then (a):(incoming a (xs)) else incoming a (xs)

-- Inserts node b to Dag after given node a
-- insert :: DAGLike -> a -> b -> DAGLike
-- insert Empty _ _ = Empty
-- insert (Node x []) Empty Empty = x
-- insert (Node x) Empty (Node b _) = error "Invalid DAG insert operation"
-- insert (Node x s@(y:ys)) (Node a _) (Node b _)
--   | x == a = (b:s)
--   | otherwise = insert' s L

traverseDAG :: DAG a -> [a]
traverseDAG (Elem a   )   = [a]
traverseDAG (List (x:xs)) = traverseDAG x ++ traverseDAG (List xs)
traverseDAG (List [])     = []

-- Tests variables
secret = "whatisup"
triplets = ["tup"
           ,"whi"
           ,"tsu"   -- tup -> tsup
           ,"ats"   -- atsup
           ,"hap"   -- hatsup
           ,"tis"   -- hatisup
           ,"whs"    
           ]

 
-- dag = [
--     ["wh"]
--     ,["ha","hi","hs"]
--     ,["at","ap"]
--     ,["is"]
--     ,["ts","ti","tu"]
--     ,["su","up"]
--   ]
s = createDuplets $ sort triplets
d = getUnique triplets
(a,b) = createDictionary triplets
-- i1 = incoming b s
-- i2 = incoming a s
-- z = incoming d s
