import Text.Read 

calc :: String -> Double
calc s = evaluateExpression (map (:[]) (validate s))

-- Validate before converting to postfix
validate :: String -> String
validate [] = "" 
validate (x:[]) = x:validate []
validate (x:y:[]) = x:y:validate []
validate (x:xs)
  -- If minus sign, compare with previous operator
  | x == '-' = validate' prev xs acc where
    validate' :: Char -> String -> String
    validate' _ [] acc = validate acc
    validate' '-' xs acc = error "Error Invalid" 
  | otherwise = validate acc

   
  
-- Calculates 
evaluateExpression :: [String] -> Double
evaluateExpression xs = evaluateExpression' xs [] where
  evaluateExpression' :: [String] -> [Double] -> Double
  evaluateExpression' [] accStack = if length accStack > 0 then (accStack!!0) else 0
  -- evaluateExpression' [x] _ = if (readMaybe x:: Maybe Double) /= Nothing then read x :: Double else error "Invalid value"    -- If only one element
  evaluateExpression' (x:xs) accStack
    | (readMaybe x :: Maybe Double) /= Nothing = evaluateExpression' xs ((read x::Double):accStack)
    | elem (head x) "*/+-" = evaluateExpression' xs ((singleEval (accStack !! 1) (accStack !! 0) (head x)):(drop 2 accStack))
    | x == " " = evaluateExpression' xs accStack   -- skip if empty spaces (this function is called with postfix expression and empty spaces)
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
-- x1 = evaluateExpression r1

-- t2 = "-123"   
-- p2 = words $ tokenize t2
-- r2 = toPostfix p2 [] []
-- x2 = evaluateExpression r2

-- t3 = "1-2*3"   
-- p3 = words $ tokenize t3
-- r3 = toPostfix p3 [] []      -- 2 3 * 1 - 
-- x3 = evaluateExpression r3   

-- t4 = "1*2+3"   
-- p4 = words $ tokenize t4
-- r4 = toPostfix p4 [] []
-- x4 = evaluateExpression r4

-- t5 = "4 - 2/(1+1)"   
-- p5 = words $ tokenize t5
-- r5 = toPostfix p5 [] []
-- x5 = evaluateExpression r5

-- 1+2-(3*4)


