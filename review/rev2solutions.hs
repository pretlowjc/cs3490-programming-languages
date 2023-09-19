{-
1. Write a function which returns the minimum element of a list. If the list is
empty, you should return 0.
-}

minList :: [Integer] -> Integer
minList [] = 0
minList [x] = x
minList (x:xs) = min x (minList xs)


{- 
2. Write a function which adds together the absolute value of all the elements
of a list.

addAbs [1,-2,3,-4] = 10
addAbs [1,10,-100] = 111
-}

addAbs :: [Integer] -> Integer
addAbs [] = 0
addAbs (x:xs) = abs x + addAbs xs

{-
3. Write a function which returns True if there exists an odd element in the
list, and returns False otherwise.
-}

existsOdd :: [Integer] -> Bool
existsOdd [] = False
existsOdd (x:xs) = odd x || existsOdd xs

{-
4. Write a function which returns Just x if there is some x in the input that
is odd, and returns Nothing otherwise.
-}

findOdd :: [Integer] -> Maybe Integer
findOdd [] = Nothing
findOdd (x:xs) | odd x = Just x
findOdd (x:xs) = findOdd xs

{-
5. Write a function which takes a list of strings and removes all empty strings
from the list.

removeEmpty ["Hello","","there","","","world",""] = ["Hello","there","world"]
-}

removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty (x:xs) | null x = removeEmpty xs
removeEmpty (x:xs) = x : removeEmpty xs

{-
6. Write a function which takes a list of pairs of integers, and outputs a list
consisting of their differences:

subtractEach [(5,1),(6,5),(7,10),(8,15),(9,20)] = [4,1,-3,-7,-11]
-}

subtractEach :: [(Integer,Integer)] -> [Integer]
subtractEach [] = []
subtractEach ((x1,x2) : xs) = (x1-x2) : subtractEach xs

{-
7. Write a function which tkaes a string name wrapped in a Maybe type, and outputs
"Hello, name!". If no name is provided, it should output "Hello!"

makeGreeting (Just "Jesse") = "Hello, Jesse!"
makeGreeting Nothing = "Hello!"
-}

makeGreeting :: Maybe String -> String
makeGreeting Nothing = "Hello!"
makeGreeting (Just s) = "Hello, " ++ s ++ "!"

{-
8. Write a function which collects all non-Nothing values in the given list and
puts them into a list of type[a].

catMaybes [Nothing, Just 1, Just 5, Nothing, Nothing, Just 2] = [1,5,2]
-}

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x :xs) = x : catMaybes xs

{-
9. Write a function which takes a list of "Either" type and returns a pair of
lists. All Left values should go into the first list in the pair, and all 
Right value should go into the second:

classify [Right 3, Left "hi", Left "there", Right 10, Right 4] = (["hi", "there"], [3,10,4])
classify [Left 1, Right ’y’, Left 5, Left 8, Right ’e’, Right ’s’] = ([1,5,8],"yes")
-}

classify :: [Either a b] -> ([a],[b])
classify [] = ([],[])
classify (Left  x : zs) = (x:p1 , p2) where (p1,p2) = classify zs
classify (Right y : zs) = (p1 , y:p2) where (p1,p2) = classify zs

{-
10. Write a function that returns true if the first list is a prefix of the second 
list, and returns False otherwise.

isPrefix "hey" "hello" = False
isPrefix [1,2] [1,2,3,4,5] = True
-}

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix (x:xs) (y:ys) = (x==y) && isPrefix xs ys
isPrefix (_:_) [] = False
