fib :: Integer -> Integer
fib n 
  | n < 0 = (-1)^(neg+1)*fib(neg) -- if negative
  | n==0 = 0
  | n==1 || n==2 = 1
  | mod n 2 == 1 = x^2 + y^2
  | otherwise = (2*w + z)*z where
    k=div (n+1) 2       
    k'=div n 2
    neg = negate n
    x=fib(k)
    y=fib(k-1)
    z=fib(k')
    w=fib(k'-1)

 