data LinkedList a = 
  Empty
  | Node a (LinkedList a)   -- Node value near
  deriving (Show)

-- List Traversal
traverseList :: LinkedList a -> [a]
traverseList Empty = [] 
traverseList (Node a near) = [a] ++ traverseList near