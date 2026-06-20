data BinaryTree a = 
    Empty
    | Node (BinaryTree a) a (BinaryTree a)
    | Leaf a 
    deriving (Show)

preOrderTraversal :: BinaryTree a -> [a]
preOrderTraversal Empty = []
preOrderTraversal (Leaf a) = [a]
preOrderTraversal (Node left a right) = [a] ++ preOrderTraversal left ++ preOrderTraversal right

inOrderTraversal :: BinaryTree a -> [a]
inOrderTraversal Empty = []
inOrderTraversal (Leaf a) = [a]
inOrderTraversal (Node left a right) = inOrderTraversal left++ [a] ++ inOrderTraversal right
  