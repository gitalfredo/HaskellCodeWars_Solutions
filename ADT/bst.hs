data BST a = 
    Empty
    | Node (BST a) a (BST a)
    | Leaf a 
    deriving (Show,Eq,Ord) 

inOrderTraversal :: BST a -> [a]
inOrderTraversal Empty = []
inOrderTraversal (Leaf a) = [a]
inOrderTraversal (Node left a right) = inOrderTraversal left++ [a] ++inOrderTraversal right
 
-- insertNode :: BST a -> a -> BST a
-- insertNode Empty _ = Empty
-- -- Reach a leaf check LT add to right
-- insertNode (Leaf a) c = if c>a then (Node Empty a (Leaf a)) else (Node (Leaf a) c (Empty))
-- insertNode (Node left a right) c = if c>a then insertNode left++ (insertNode a) ++insertNode right