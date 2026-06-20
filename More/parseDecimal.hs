import Data.List
import Data.Maybe
-- Parse decimal
get :: String -> [Int]
get s = get' s "" where
    get' [] _ = []
    get' (x:xs) lastMultiplier 
        | elem x $ map (fst) $ zip sUnits vUnits = vUnits !! (fromJust $ elemIndex x sUnits : get' xs lastMultiplier)
        | elem x $ map (fst) $ zip sTens vTens = vTens !! (fromJust $ elemIndex x sTens : get' xs lastMultiplier)
        | elem x $ map (fst) $ zip sMultipliers vMultipliers = vMultipliers !! (fromJust $ elemIndex x sMultipliers : get' xs x)

-- Units as string and value from 1-19
sUnits = ["one","two","three","four","five","six","seven","eight","nine","ten"
    ,"eleven","twelve","thirdteen","fourteen","fifteen","sixteen","seventeen","eightteen","nineteen"]
vUnits = [1..19]

-- Tens
sTens = ["twenty","thirty","fourty","fifty","sixty","seventy","eighty","ninety"]
vTens = [i*10 | i<-[2..9]]

-- Multipliers
sMultipliers = ["hundred","thousands"]
vMultipliers = [100,1000]

-- eg  "two hundred eighty-three"
-- find tens: elem '-' $ words s
-- OR fst elem contains: elem "twenty" $ map (fst) $ zip sTens vTens

