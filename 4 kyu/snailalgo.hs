snail :: [[Int]] -> [Int]
snail [[]] = []
snail xs = snail' xs (Coord 0 0) (Dir 'E') [] [(Coord 0 0)] where
  -- If current coordinates are empty, return to the last visited and change direction again
  snail' _ Empty (Dir lastDir) accValues visited = snail' xs (visited!!0) (changeDir (Dir lastDir)) accValues visited     
  snail' xs (Coord x y) (Dir lastDir) accValues visited
    | countNeighbors xs (Coord x y) visited > 0 = if (move xs (Coord x y) (Dir lastDir) visited) /= Empty then
    snail' xs (move xs (Coord x y) (Dir lastDir) visited) (Dir lastDir) (accValues++[((xs !! x) !! y)]) ((Coord x y):visited) else
    -- If cannot move, then change to dir
    snail' xs (Coord x y) (changeDir (Dir lastDir)) accValues visited
    | otherwise = (accValues++[((xs !! x) !! y)]) -- adds the last elem

data Coord = Coord Int Int | Empty 
    deriving (Show,Eq)
data Neighbors = Neighbors Coord Coord Coord Coord deriving (Show) -- NESW
data Dir = Dir Char deriving (Show) 

getNeighbors :: [[Int]] -> Coord -> [Coord] -> Neighbors
getNeighbors _ Empty _ = Neighbors Empty Empty Empty Empty
getNeighbors xs (Coord a b) visited = Neighbors (validate xs (Coord (a - 1) b) visited)
 (validate xs (Coord a (b + 1)) visited) 
 (validate xs (Coord (a + 1) b) visited) 
 (validate xs (Coord a (b - 1)) visited)

-- Validates bounds 
validate :: [[Int]] -> Coord -> [Coord] -> Coord
validate _ Empty _ = Empty
validate xs (Coord x y) visited
  | x < 0 || y < 0 || length xs <= x || length xs <= y || elem (Coord x y) visited == True = Empty
  | otherwise = Coord x y

move :: [[Int]] -> Coord -> Dir -> [Coord] -> Coord
move _ Empty _ _ = Empty
move xs (Coord x y) (Dir dir) visited
  | dir == 'N' = validate xs (Coord (x-1) y) visited
  | dir == 'E' = validate xs (Coord x (y+1)) visited  
  | dir == 'S' = validate xs (Coord (x+1) y) visited 
  | dir == 'W' = validate xs (Coord x (y-1)) visited 
  | otherwise = error "Error: move direction not NESW"      -- This line is not supposed to run

-- Changes direction clockwise
changeDir :: Dir -> Dir
changeDir (Dir dir) 
  | dir == 'E' = Dir 'S'
  | dir == 'S' = Dir 'W'
  | dir == 'W' = Dir 'N'
  | dir == 'N' = Dir 'E'
  | otherwise = error "Error: change direction not NESW"
 
countNeighbors :: [[Int]] -> Coord -> [Coord] -> Int
countNeighbors xs (Coord x y) visited = counter (getNeighbors xs (Coord x y) visited) visited where
  counter (Neighbors a b c d) visited = sum $ map (\ e-> if e == Empty || elem e visited then 0 else 1) [a,b,c,d]


-- Tests
arr = [[1,2,3],[4,5,6],[7,8,9]];
x = snail(arr);