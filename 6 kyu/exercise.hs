import Data.Char
alphabetPosition :: String -> String
alphabetPosition [] = ""
alphabetPosition xs = displayAsString (mix [i | i<-(map toLower xs), i `elem` ['a'..'z']])

displayAsString :: [Int] -> String
displayAsString xs = unwords (show <$> xs)

check :: Int -> Char -> String -> Int
check n c [] = -1
check n c xs = if (head xs)==c then n else check (n+1) c (tail xs)

-- Calls check many times
mix :: String -> [Int]
mix [] = []
mix xs = check 1 (head xs) ['a'..'z']: mix (tail xs)