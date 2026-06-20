module Baker where

type Ingredient = String
type Amount     = Integer
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage = bake recipe storage 0 where
  -- bake 1 cake
  cake' [] s = s
  cake' r@(x:xs) s
    -- Return [] if ingredient not found
    -- or not enough in storage
    | not(elem (fst x) (map fst s)) || length [i | i<-s,ii<-r, fst i==fst ii, snd i>=snd ii && snd i /= 0] /= length r = []   
    | otherwise = cake' xs [if (fst i==fst x && snd x<=snd i) then (fst i,snd i-snd x ) else i | i<-s]
  -- bake all cakes and keep the count
  bake [] _ acc = acc 
  bake _ [] acc = acc       -- Cake failed
  bake r s acc = if cake' r s /= [] then bake r (cake' r s) (acc+1) else acc

 

-- Tests 
-- s = [("a",1),("b",2),("c",3)]
-- r = [("b",1)]
r' = [("flour",500), ("sugar",200), ("eggs",1)]
s' = [("flour",1200), ("sugar",1200), ("eggs",5), ("milk",200)]
-- s'' = [("b",5),("d",1)]
-- cakes [(fst i, fromIntegral $ snd i) | i<-s'] [(fst i, fromIntegral $ snd i) | i<-s]

cake [] s = s
cake r@(x:xs) s
  -- Return [] if ingredient not found
  -- or not enough in storage
  | not(elem (fst x) (map fst s)) || length [i | i<-s,ii<-r, fst i==fst ii, snd i>=snd ii && snd i /= 0] /= length r = [] 
  | otherwise = cake xs [if (fst i==fst x && snd x<=snd i) then (fst i,snd i-snd x ) else i | i<-s]

-- cake  [("flour",500), ("sugar",200), ("eggs",1)] [("flour",0), ("sugar",0), ("eggs",5), ("milk",200)]      
-- shouldBe []