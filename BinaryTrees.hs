module BinaryTrees where

data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
  deriving (Eq, Show)

left :: Tree a -> Maybe a
left Empty = Nothing
left (Node Empty k _) = Just k
left (Node l _ _) = left l

reverseTree :: Tree a -> Tree a
reverseTree Empty = Empty
reverseTree (Node l k r) =
  Node (reverseTree r) k (reverseTree l)

-- Testing the validity
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node t a u) = inorder t ++ [a] ++ inorder u

-- t = Node (Node Empty 3 Empty) 6 (Node Empty 7 (Node Empty 10 Empty))
-- a = inorder (reverseTree t)
-- b = reverse (inorder t)
