-- TODO: [CLEAN], accept and finish 
spiral :: Int -> [[Int]]
spiral n = spiral' (init n) n (Coord 0 0) (Dir 'E') 0 where
  init n = visited (create n) n (Coord 0 0)   
  -- Calls move and visited 
  -- Total change dir == dims
  spiral' grid dims (Coord x y) (Dir dir) counterChangeDirs
    | counterChangeDirs == dims - 1 = grid
    | otherwise = if move grid (Coord x y) (Dir dir) /= Empty 
      then spiral' (visited grid dims (nextMove (Dir dir))) dims (nextMove (Dir dir)) (Dir dir) counterChangeDirs
      else spiral' (visited grid dims (nextMove (changeDir (Dir dir)))) dims (nextMove (changeDir (Dir dir))) (changeDir (Dir dir)) (counterChangeDirs + 1) where
        nextMove dir = move grid (Coord x y) dir

data Coord = Coord Int Int | Empty 
    deriving (Show,Eq) 
data Dir = Dir Char deriving (Show)   

move :: [[Int]] -> Coord -> Dir -> Coord
move _ Empty _ = Empty
move grid (Coord x y) (Dir dir) 
  | dir == 'N' = if x-1>=0 then Coord (x-1) y else Empty
  | dir == 'E' = if y+1<=length grid then Coord x (y+1) else Empty
  | dir == 'S' = if x+1<=length grid then Coord (x+1) y else Empty
  | dir == 'W' = if y-1>=0 then Coord x (y-1) else Empty
  | otherwise = error "Error: move direction not NESW"      -- This line is not supposed to run

 
-- Creates grid, given nxn matrix dims
create :: Int -> [[Int]]
create n = create' n n where
  create' :: Int -> Int -> [[Int]]
  create' _ 0 = []
  create' n m = (take n (cycle [0])):(create' n (m-1))


-- Changes direction clockwise
changeDir :: Dir -> Dir
changeDir (Dir dir) 
  | dir == 'E' = Dir 'S'
  | dir == 'S' = Dir 'W'
  | dir == 'W' = Dir 'N'
  | dir == 'N' = Dir 'E'
  | otherwise = error "Error: change direction not NESW"

-- Change cell to visited (copies the entire matrix to modify a value)
-- dims is NxN, m n is the location for the visited value
visited :: [[Int]] -> Int -> Coord -> [[Int]]
visited grid dims (Coord m n) = visited' grid dims (Coord m n) 0 where
  visited' [] _ _ _ = []
  visited' grid@(x:xs) dims (Coord m n) counter
    | counter==dims-1 = []
    | counter==m = (setListItem x n) : visited' xs dims (Coord m n) (counter+1)
    | otherwise = (x:visited' xs dims (Coord m n) (counter+1))
 
setListItem xs n = if n<length xs then take n xs ++ [1] ++ drop (n+1) xs else xs
 