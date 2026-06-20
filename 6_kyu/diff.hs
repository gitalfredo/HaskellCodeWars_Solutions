difference :: Eq a => [a] -> [a] -> [a]
difference a b = [i | i<-a, not (i `elem` b) ]

 