likes :: [String] -> String
likes []        = "no one likes this"
likes [a]       = a ++ " likes this"
likes [a, b]    = a ++ " and " ++ b ++ " like this"
likes [a, b, c] = a ++ ", " ++ b ++ " and " ++ c ++ " like this"
likes (a:b:c)   = a ++ ", " ++ b ++ " and "++ show (length c) ++" others like this"   -- firt, second, and array
 
