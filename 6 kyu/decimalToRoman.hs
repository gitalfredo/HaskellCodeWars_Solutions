-- Decimal number to roman
solution :: Integer -> String
solution n = calculate n "" where
    calculate m acc     
        | m>0  = calculate (m-(snd (getTuple m))) (acc++(fst (getTuple m)))     -- Keep subtracting highest value smaller than the current number
        | m==0 = acc

getTuple :: Integer -> (String, Integer)
getTuple n = last [i | i <- table, snd i <= n]  -- Get highest tuple that is smaller than the current number


-- Conversion table extended to make for IX and CD, and CM...etc
table=[("I",1),("IV",4),("V",5),("IX",9),("X",10),("XL",40),("L",50),("XC",90),("C",100),("CD",400),("D",500),("CM",900),("M",1000)]
