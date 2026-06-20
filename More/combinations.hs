import Data.List
-- permutations :: String -> [String]
-- permutations x:xs = x:permutations(xs)

-- Every set contains a unique empty subset.
subsets 0 _ = [[]]

-- Empty sets don't have any (non-empty) subsets.
subsets _ [] = []

-- Otherwise we're dealing with non-empty subsets of a non-empty set.
-- If the first element of the set is x, we can get subsets of size n by either:
--   - getting subsets of size n-1 of the remaining set xs and adding x to each of them
--     (those are all subsets containing x), or
--   - getting subsets of size n of the remaining set xs
--     (those are all subsets not containing x)
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs

combinations k ns = nub . filter ((k==).length) $ subsequences ns

-- Product rule then filter
productRule :: String -> [String]
productRule s = sequence $ replicate (length s) s

 