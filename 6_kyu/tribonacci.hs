tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = initial (a, b, c) n n where
    initial (a, b, c) n m
        | n<=0 = []
        | n==m && n>=3 = a:b:c:initial (a, b, c) (n-3) m  -- Runs a first time without performing addition
        | n==m && n==2 = a:[b]  -- Makes sure it returns n 
        | n==m && n==1 = [a]  -- Makes sure it returns n 
        | otherwise = (a + b + c): initial (b, c, (a + b + c)) (n-1) m

tribonacci' :: Num a => (a, a, a) -> Int -> [a]
tribonacci' (_,_,_) 0 = []
tribonacci' (a,b,c) n = a: tribonacci' (b, c, (a+b+c)) (n-1)