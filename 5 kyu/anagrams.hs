import Data.List (sort)
anagrams :: String -> [String] -> [String]
anagrams w ws = [i | i <- ws, sort w == sort i]





-- Tests

a = anagrams "abba"  ["aabb", "abcd", "bbaa", "dada"] -- ["aabb", "bbaa"]
b = anagrams "laser" ["lazing", "lazy",  "lacer"] --  []