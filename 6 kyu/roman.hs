solution :: String -> Int
solution s = calculate (reverse s) 'I' 0 where
    calculate (x:xs) c acc   -- Compares values in pairs if current bigger, then add, else subtract
        | getValue x >= getValue c  = calculate xs x (getValue x)+acc
        | otherwise                 = calculate xs x acc-(getValue x)
    calculate [] c acc  = acc

-- Conversion table
table=[('I',1),('V',5),('X',10),('L',50),('C',100),('D',500),('M',1000)]

-- Get roman value given a match
getValue :: Char -> Int
getValue c = snd $ filter ((== c).fst) table !! 0
