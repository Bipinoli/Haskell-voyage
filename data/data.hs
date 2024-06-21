data Calculation = Add Int Int | Sub Int Int

calc :: Calculation -> Int
calc (Add x y) = x + y
calc (Sub x y) = x - y

data Tree a = Leaf | Node (Tree a) a (Tree a)

tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node (Node Leaf 3 Leaf) 4 Leaf)

data PeaNum = Succ PeaNum | Zero
