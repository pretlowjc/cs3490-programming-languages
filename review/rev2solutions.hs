minList :: [Integer] -> Integer
minList [] = 0
minList [x] = x
minList (x:xs) = min x (minList xs)

addAbs :: [Integer] -> Integer
addAbs [] = 0
addAbs (x:xs) = abs x + addAbs xs

existsOdd :: [Integer] -> Bool
existsOdd [] = False
existsOdd (x:xs) = odd x || existsOdd xs

findOdd :: [Integer] -> Maybe Integer
findOdd [] = Nothing
findOdd (x:xs) | odd x = Just x
findOdd (x:xs) = findOdd xs

removeEmpty :: [String] -> [String]
removeEmpty [] = []
removeEmpty (x:xs) | null x = removeEmpty xs
removeEmpty (x:xs) = x : removeEmpty xs

subtractEach :: [(Integer,Integer)] -> [Integer]
subtractEach [] = []
subtractEach ((x1,x2) : xs) = (x1-x2) : subtractEach xs

makeGreeting :: Maybe String -> String
makeGreeting Nothing = "Hello!"
makeGreeting (Just s) = "Hello, " ++ s ++ "!"

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x :xs) = x : catMaybes xs

classify :: [Either a b] -> ([a],[b])
classify [] = ([],[])
classify (Left  x : zs) = (x:p1 , p2) where (p1,p2) = classify zs
classify (Right y : zs) = (p1 , y:p2) where (p1,p2) = classify zs

isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix (x:xs) (y:ys) = (x==y) && isPrefix xs ys
isPrefix (_:_) [] = False
