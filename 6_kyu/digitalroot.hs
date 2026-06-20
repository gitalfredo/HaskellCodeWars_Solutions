-- sum of the digits of a number
digitalRoot :: Integral a => a -> a
digitalRoot a = if length (show (reduce a))>1 then digitalRoot (reduce a) else reduce a
    where 
    reduce x = fromInteger $ sum (map read $ convertToList $ show $ toInteger x :: [Integer])
    convertToList s = map (:[]) s 

-- formula at https://en.wikipedia.org/wiki/Digital_root
digitalRoot' :: Integral a => a -> a
digitalRoot' 0 = 0
digitalRoot' a = 1+(a-1) `mod` 9