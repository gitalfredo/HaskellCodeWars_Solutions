sumOfIntegersInString :: [Char] -> Int
sumOfIntegersInString xs = sum (map read filteredNumbers::[Int]) where
  filteredNumbers = words [if elem i ['0'..'9'] then i else ' ' | i<-xs]

-- Tests
t="The30quick20brown10f0x1203jumps914ov3r1349the102l4zy dog"