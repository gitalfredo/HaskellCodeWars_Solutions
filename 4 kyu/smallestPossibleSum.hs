-- if X[i] > X[j] then X[i] = X[i] - X[j]
smallestPossibleSum :: (Integral a) => [a] -> a
smallestPossibleSum [x] = x
smallestPossibleSum xs = sum (take (length xs) (cycle [helper xs])) where
  helper xs = foldl (gcd) 0 xs  -- Continuous subtraction means division

-- Tests **********************
xs1=[6,9,21]
t=smallestPossibleSum xs1
 

