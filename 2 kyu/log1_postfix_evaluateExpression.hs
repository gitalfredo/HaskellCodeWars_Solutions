import Text.Read 

calc :: String -> Double
calc s = evaluatePostfix (toPostfix (words (tokenize s)) [] []) 

-- Validate before converting to postfix
validate :: String -> String
validate [] = ""
validate (x:[]) = x:validate []
validate (x:y:[]) = x:' ':y:validate []
validate (x:y:z:xs)
  | x == '-' && y == ' ' && z == '(' = error "Invalid"
  | x == '-' && y == ' ' && elem x "0123456789" = error "Invalid"
  -- | x == '-' && y == '-' = error "Invalid" 
  | x == '-' && elem y "0123456789" = x:y:' ':validate (z:xs)
  | otherwise = x:' ':validate (y:z:xs)


-- Call helper to substitute expression within parenthesis for its postfix evaluation
tokenize :: String -> String 
tokenize (x:y:xs)
  | elem x "*/=-" && y == '(' = tokenize' (x:xs) [] x 
  | otherwise = x:tokenize (y:xs)
  where
    tokenize' (x:xs) acc c
      -- Process expression within parenthesis (already accumulated)
      | x == ')' = (show (evaluatePostfix (toPostfix (words (validate acc)) [] [])))++tokenize xs
      -- Accumulate everything within parenthesis
      | otherwise = tokenize' xs (x:acc) c
    

 
-- Input tokenize expression: ["1","2","+"]
toPostfix :: [String] -> [String] -> [String] -> [String]
toPostfix [] operators nums 
  | length operators > 0 && length nums > 0 = nums++operators 
  | otherwise = []
toPostfix (x:xs) operators nums 
  -- if +- with previous data (a op b), then previous operation was also +-
  | (x == "+" || x == "-") && length nums > 1 && length operators > 0 = (nums !! 1) : (nums !! 0) : (operators !! 0):(toPostfix xs [x] []) 
  -- If no prev data, then add operator
  | (x == "*" || x == "/" || x == "+" || x == "-") = toPostfix xs (x:operators) nums   
  | x == " " = toPostfix xs operators nums      -- skips empty spaces
  -- if something like 4 * 5 then execute, else 4+5 store numbers and op in case a higher priority operator shows up (*/)
  | (readMaybe x::Maybe Double) /= Nothing && length nums > 0 && length operators > 0 = if elem (head (operators !! 0)) "*/" && length operators == 1 then
    (nums !! 0) : x : (operators !! 0):toPostfix xs [] [] 
    else if elem (head (operators !! 0)) "*/" && length operators == 2 then (nums !! 0) : x : (operators !! 0) : (nums !! 1) : (operators !! 1):toPostfix xs [] [] 
    else toPostfix xs operators (x:nums)
  -- if number but the previous operation was */ following +- (eg 1+2*3)
  -- | elem (head x) "0123456789" && length nums == 2 && length operators == 1 = (nums !! 0) : x : (operators !! 0):(nums !! 1) : (operators !! 1):toPostfix xs [] []
  | (readMaybe x::Maybe Double) /= Nothing = toPostfix xs operators (x:nums)
  | otherwise = error "Error unknown symbol" 
  
-- Calculates postfix 
evaluatePostfix :: [String] -> Double
evaluatePostfix xs = evaluatePostfix' xs [] where
  evaluatePostfix' :: [String] -> [Double] -> Double
  evaluatePostfix' [] accStack = if length accStack > 0 then (accStack!!0) else 0
  -- evaluatePostfix' [x] _ = if (readMaybe x:: Maybe Double) /= Nothing then read x :: Double else error "Invalid value"    -- If only one element
  evaluatePostfix' (x:xs) accStack
    | (readMaybe x :: Maybe Double) /= Nothing = evaluatePostfix' xs ((read x::Double):accStack)
    | elem (head x) "*/+-" = evaluatePostfix' xs ((singleEval (accStack !! 1) (accStack !! 0) (head x)):(drop 2 accStack))
    | x == " " = evaluatePostfix' xs accStack   -- skip if empty spaces (this function is called with postfix expression and empty spaces)
    | otherwise = error "Error invalid evaluation"

singleEval :: Double -> Double -> Char -> Double
singleEval a b op
  | op=='*' = a * b
  | op=='/' = a / b
  | op=='+' = a + b
  | op=='-' = a - b
                
       
-- Tests 
-- t1 = "2 /2+3 * 4.75- -6"   
-- p1 = words $ tokenize t1
-- r1 = toPostfix p1 [] []     -- 2 2 / 3 4.75 * + (-6) -
-- x1 = evaluatePostfix r1

-- t2 = "-123"   
-- p2 = words $ tokenize t2
-- r2 = toPostfix p2 [] []
-- x2 = evaluatePostfix r2

-- t3 = "1-2*3"   
-- p3 = words $ tokenize t3
-- r3 = toPostfix p3 [] []      -- 2 3 * 1 - 
-- x3 = evaluatePostfix r3   

-- t4 = "1*2+3"   
-- p4 = words $ tokenize t4
-- r4 = toPostfix p4 [] []
-- x4 = evaluatePostfix r4

-- t5 = "4 - 2/(1+1)"   
-- p5 = words $ tokenize t5
-- r5 = toPostfix p5 [] []
-- x5 = evaluatePostfix r5

-- 1+2-(3*4)


