import Data.List (sort)

-- Example bubble sort 
orderWeight :: [Char] -> [Char]
orderWeight strng = unwords ( bubble (words strng)) -- Converts/join array back to string
  
-- Single weight new value
findWeight :: [Char] -> Int
findWeight str = sum(converter str) where
    converter []    = []
    converter (c:s) = (read [c] :: Int):converter s     -- Converts string to int array

-- One pass of bubble sort
onePass :: [String] -> [String]
onePass []       = []
onePass [x]      = [x]
onePass [x,y]    
    | (findWeight x < findWeight y)   = x:y:onePass []
    | (findWeight x == findWeight y)  = (sort [x, y]) 
    | otherwise                     = y:x:onePass []
onePass (x:y:xs)
    | (findWeight x < findWeight y)   = x:onePass (y:xs)
    | (findWeight x == findWeight y)  = (head (sort [x, y])):onePass ((last (sort [x, y])):xs)
    | otherwise                     = y:onePass (x:xs)

bubble :: [String] -> [String]
bubble xs = if (onePass xs) == xs then xs else (bubble (onePass xs))