import Data.List (sortBy)
import Data.Ord  (comparing)
import Data.Char (toLower)

top3 :: [Char] -> [[Char]]
top3 s = split (map toLower s) "" []

punctuation = ["#", "\\", "/","."," ",",",":",";","!","?","_","-"]

-- let s = "a a a  b  c c  d d d d  e e e e e"

split :: [Char] -> [Char] -> [([Char],Int)] -> [[Char]]
split [] _ acc = map fst $ take 3 (reverse $ sortBy ( comp) acc)
split (x:xs) token acc
    -- x in punctuation             token found in accumulator then update value count
    | elem (x:"") punctuation && token/="" = if any ((==token).fst) acc then (split xs "" $ increment acc) else if validateToken token then split xs "" ((token,0):acc) else split xs "" acc
    | elem (x:"") punctuation && token=="" = split xs "" acc -- Continuous punctuation
    | otherwise                            = split xs (token++(x:"")) acc        -- Add char to token end (as suffix)
    where 
        increment [] = []
        increment (y:ys) = if (fst y == token) then (fst y, snd y + 1):ys else y:increment ys
        validateToken t = [i|i<-t,elem i ['a'..'z']]/=""

-- Comparer function
comp (a1, b1) (a2, b2)
  | b1 > b2 = GT
  | b1 < b2 = LT
  | b1 == b2 = compare a1 a2

-- Tests
-- Test variables
test1 = unlines
      [ "In a village of La Mancha, the name of which I have no desire to call to"
      , "mind, there lived not long since one of those gentlemen that keep a lance"
      , "in the lance-rack, an old buckler, a lean hack, and a greyhound for"
      , "coursing. An olla of rather more beef than mutton, a salad on most"
      , "nights, scraps on Saturdays, lentils on Fridays, and a pigeon or so extra"
      , "on Sundays, made away with three-quarters of his income."
      ]