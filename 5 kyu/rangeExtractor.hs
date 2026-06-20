import Data.List (sort, nub)
solution :: [Integer] -> String
solution [] = ""
solution [x]= show x
solution [x,y]= show x ++ "," ++ show y
solution (xs)   
  | counter>=2 = show (head xs) ++ "-" ++ (show $ xs !! counter) ++ (if length xs-counter-1>0 then "," else "") ++ solution (drop (counter+1) xs)    
  | otherwise = show (head xs) ++ "," ++ solution (tail xs)
  where
  counter = fromInteger $ countAdjacent xs
-- Count how many more adjacent integers there are in range 
countAdjacent :: [Integer] -> Integer
countAdjacent [] = 0
countAdjacent [a,b] = if a+1==b then 1 else 0
countAdjacent xs = if (head xs)+1==(head $ tail xs) then 1+(countAdjacent (tail xs)) else 0
 

-- Test variables
t1 = [-10, -9, -8, -6, -3, -2, -1, 0, 1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20]
r1 = solution t1        -- returns "-10--8,-6,-3-1,3-5,7-11,14,15,17-20"
a1 = countAdjacent t1

t2 = [-6,-3,-2,-1,0,1,3,4,5,7,8,9,10,11,14,15,17,18,19,20]
r2 = solution t2        -- returns "-6,-3-1,3-5,7-11,14,15,17-20"

t3 = [42,28,28,11,25,27,26,21,3,32,46,27,19,1,15,30,2,26,15,28,39,40,46,18,10,47,39,0,4,5,27,27,37,12,27,27,44,8,27,21] 
-- expected "0-5,8,10-12,15,18,19,21,25-28,30,32,37,39,40,42,44,46,47" 
-- but got "0-5,8,10-12,15,18,19,21,25-28,30,32,37,39,40,42,44,.46,47"
r3 = solution (nub (sort t3))