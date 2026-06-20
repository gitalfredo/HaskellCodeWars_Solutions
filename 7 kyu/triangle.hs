import Data.List (sort)

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = a > 0 && b > 0 && c > 0 && a+b>c && b+c>a && a+c>b

isTriangle' :: Int -> Int -> Int -> Bool
isTriangle' a b c = case sort [a, b, c] of 
    [min, mid, max] -> min + mid > max