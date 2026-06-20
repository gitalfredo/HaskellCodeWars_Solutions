findEvenIndex :: [Int] -> Int
findEvenIndex arr = splitter arr 0 where
    splitter arr n 
        | sum (take n arr) == sum (drop (n+1) arr)  = n       -- Base case success
        | n > (length arr)-1                        = -1      -- Base case fail to find even index 
        | otherwise                                 = splitter arr (n+1)