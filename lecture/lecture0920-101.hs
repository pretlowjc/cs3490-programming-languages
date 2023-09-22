data List a = Empty | Cons a (List a)
  deriving Show

-- [1,2,6] = 1 : 2 : 6 : []
ex :: List Integer
ex = Cons 1 (Cons 2 (Cons 6 Empty))

addList :: List Integer -> Integer
addList Empty = 0
addList (Cons x xs) = x + addList xs

findInt :: Integer -> List Integer -> Bool
findInt n Empty       = False
findInt n (Cons x xs) = if n == x then True else findInt n xs
-- findInt n (Cons x xs) = n == x || findInt n xs

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
