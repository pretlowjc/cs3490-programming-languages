checkString :: String -> [String] -> Bool
checkString x []     = False
checkString x (y:ys) = if x == y then True else checkString x ys

checkInt :: Integer -> [Integer] -> Bool
checkInt x [] = False
checkInt x (y:ys) = if x == y then True else checkInt x ys

checkElem :: (Eq a) => a -> [a] -> Bool
checkElem x [] = False
checkElem x (y:ys) = if x == y then True else checkElem x ys

myFun :: Integer -> Integer -> Integer
myFun x y = (x+y)^2 - x*y

listOfFuns :: [Integer -> Integer -> Integer]
listOfFuns = [(+), (*) , mod]

-- Merge sort

-- merge : combine two *sorted* lists into a single, sorted list
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys         = ys
merge xs []         = xs
merge (x:xs) (y:ys) = if x < y then x : (merge xs (y:ys))
                               else y : (merge (x:xs) ys)

splitList :: [a] -> ([a],[a])
splitList [] = ([],[])
splitList (x:[]) = ([x],[]) -- same as [x]
splitList (x:(y:ys)) = let (list1,list2) = splitList ys
                        in (x:list1 , y:list2)

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = let (ys,zs) = splitList xs
                in merge (mergeSort ys) (mergeSort zs)

-- Insertion sort
-- insert: takes a sorted list, and a new element, and returns a new list
--         with that element in the right place

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x : (y : ys)
                | otherwise = y : insert x ys

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

-- Either example
-- Say a function takes [Either String Integer]
-- Returns the sum of all integers and lengths of all strings
addIntStr :: [Either String Integer] -> Integer
addIntStr []     = 0
addIntStr (Left s  : xs) = fromIntegral (length s) + addIntStr xs
addIntStr (Right n : xs) = n + addIntStr xs
