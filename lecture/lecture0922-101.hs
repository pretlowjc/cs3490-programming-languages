map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

fold :: (a -> b -> b) -> b -> [a] -> b
fold rf bc [] = bc
fold rf bc (x:xs) = rf x (fold rf bc xs)

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = fold (\x y -> f x : y) ([]) (xs)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p xs = fold (\x y -> if (p x) then (x : y) else (y) ) ([]) (xs)

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

exTree :: Tree Integer
exTree = Node 5 (Node 3 Leaf Leaf)
                (Node 7 (Node 0 Leaf Leaf)
                        (Node 1 Leaf Leaf))

addTree :: Tree Integer -> Integer
addTree Leaf = 0
addTree (Node x t1 t2) = x + addTree t1 + addTree t2

findIntTree :: Integer -> Tree Integer -> Bool
findIntTree n Leaf = False
findIntTree n (Node x t1 t2) = x == n || findIntTree n t1 || findIntTree n t2

concatTree :: Tree String -> String
concatTree Leaf = ""
concatTree (Node s t1 t2) = concatTree t1 ++ s ++ concatTree t2

doubleTree :: Tree Integer -> Tree Integer
doubleTree Leaf = Leaf
doubleTree (Node x t1 t2) = Node (2*x) (doubleTree t1) (doubleTree t2)

intTree2Str :: Tree Integer -> Tree String
intTree2Str Leaf = Leaf
intTree2Str (Node x t1 t2) = Node (show x) (intTree2Str t1) (intTree2Str t2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree rf bc Leaf = bc
foldTree rf bc (Node x t1 t2) = rf x (foldTree rf bc t1) (foldTree rf bc t2)

addTree' :: Tree Integer -> Integer
addTree' t = foldTree (\x y z -> x + y + z) (0) t
findIntTree' :: Integer -> Tree Integer -> Bool
findIntTree' n t = foldTree (\x y z -> n == x || y || z) (False) (t)
concatTree' :: Tree String -> String
concatTree' t = foldTree (\x y z -> y ++ x ++ z) ("") t
mapTree' :: (a -> b) -> Tree a -> Tree b
mapTree' f t = foldTree (Node . f) (Leaf) (t)
