-- index takes a list of type a and outputs a list pairs where every element is tagged by its index

index :: [a] -> [(Integer,a)]
index xs = helper xs 0

helper :: [a] -> Integer -> [(Integer,a)]
helper     [] k = []
helper (x:xs) k = (k,x) : helper xs (k+1)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = append (rev xs) [x]

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

rev' :: [a] -> [a]
-- rev' xs = revHelper [] xs
rev'    = revHelper []

revHelper :: [a] -> [a] -> [a]
revHelper acc []     = acc
revHelper acc (x:xs) = revHelper (x : acc) xs

index' :: [a] -> [(Integer,a)]
index' xs = zip [0..] xs

addLists :: [Integer] -> [Integer] -> [Integer]
addLists xs [] = []
addLists [] ys = []
addLists (x:xs) (y:ys) = (x+y) : addLists xs ys

addOne :: [Integer] -> [Integer]
addOne [] = []
addOne (x:xs) = x+1 : addOne xs
