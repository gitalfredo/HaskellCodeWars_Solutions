import Data.Char
-- | Returns true if the number of
-- Xs is equal to the number of Os
-- (case-insensitive)
xo :: String -> Bool
xo str = (sum (accumulate (map toLower str)))==0 where
    accumulate ('x':xs) = 1:accumulate xs
    accumulate ('o':xs) = -1:accumulate xs    -- +1 and -1 must break even
    accumulate (_:xs) = 0:accumulate xs       -- Neither 'x' nor 'o'
    accumulate [] = []

check :: String -> [Int]
check ('x':xs) = 1:check xs
check ('o':xs) = -1:check xs
check (_:xs) = 0:check xs
check [] = []