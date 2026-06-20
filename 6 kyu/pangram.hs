import Data.Char (toLower)
import Data.List
isPangram :: String -> Bool
isPangram str = sum (adder (filter onlyLetters (map toLower str)) [])==26 where -- adder adds chars to the accumulator
    adder (c:rest) acc
        | c `elem` acc = 0:adder rest acc         -- character already in the accumulator, keep iterating
        | otherwise    = 1:adder rest (c:acc)     -- add character if not present
    adder [] acc = []

-- Filter conditional
onlyLetters :: Char -> Bool
onlyLetters c = c `elem` ['a'..'z'] 

isPangram' :: String -> Bool 
isPangram' str = (== 26) $ length $ nub $ filter onlyLetters $ map toLower str  -- nub remove duplicates

 