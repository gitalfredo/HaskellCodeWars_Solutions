-- seconds to years, days, hours, minutes and seconds
formatDuration :: (Integral i) => i -> String
formatDuration 0 = "now"
formatDuration n = printing (formatDuration' n 0) ["years", "days", "hours", "minutes", "seconds"]
  
formatDuration' :: (Integral i) => i -> Int -> [i]
formatDuration' n 4 = [n]        -- Base case when n is checked against all items in table
formatDuration' n e = if n>=(fromInteger valE) then (div n (fromInteger valE)):formatDuration' (solveRest n (fromInteger valE)) (e+1) else 0:formatDuration' n (e+1) where
  valE = (tableSeconds !! e)

-- Display numeric result along with ["years", "days", "hours", "minutes", "seconds"]
printing :: (Integral i) => [i] -> [String] -> String
printing [] _ = ""
printing _ [] = ""
printing s@(x:xs) (y:ys)
  -- Skip durations equal to 0
  | x==0 = printing xs ys
  -- return
  | length (filterZeroes s) == 1 = (show (fromIntegral x)) ++ " " ++ (solvePlural x y) 
  | length (filterZeroes s) == 2 = (show (fromIntegral x)) ++ " " ++ (solvePlural x y)  ++ " and " ++ printing xs ys 
  -- append commas
  | length (filterZeroes s) > 2 = (show (fromIntegral x)) ++ " " ++ (solvePlural x y) ++ ", " ++ printing xs ys 


-- Table for      years,    days,  hours, minutes in seconds
tableSeconds = [365*24*3600,24*3600,3600,60] 
filterZeroes s = [i | i<-s,i/=0]
solveRest n e = if mod n e == 0 then 0 else mod n e
solvePlural x y = if x == 1 then init y else y
 
-- Tests
mockPrint :: (Integral i) => i -> String  
mockPrint n = show (fromIntegral n)

 