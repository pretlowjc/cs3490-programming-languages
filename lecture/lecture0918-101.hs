incrList :: [Integer] -> [Integer]
incrList [] = []
incrList (x:xs) = x+1 : incrList xs

reverseAll :: [String] -> [String]
reverseAll [] = []
reverseAll (x:xs) = reverse x : reverseAll xs

squareLengthAddOne :: [String] -> [Integer]
squareLengthAddOne [] = []
squareLengthAddOne (x:xs) = (fromIntegral (length x))^2 + 1 : squareLengthAddOne xs

myStr :: [String]
-- myStr = ["this", "is", "a", "long", "list", "having", "some", "strings", "in", "it"]
myStr = words "this is a long list having some strings in it"

oddsOnly :: [Integer] -> [Integer]
oddsOnly [] = []
oddsOnly (x:xs) = if odd x then x : oddsOnly xs else oddsOnly xs

oddLengthOnly :: [String] -> [String]
oddLengthOnly [] = []
oddLengthOnly (x:xs) = if odd (length x) then x : oddLengthOnly xs else oddLengthOnly xs

oddsOnly' :: [Integer] -> [Integer]
oddsOnly' = filter' odd

oddLengthOnly' :: [String] -> [String]
-- oddLengthOnly' = filter' (\x -> odd (length x))
oddLengthOnly' = filter' (odd . length)

evenLengthOnly :: [String] -> [String]
evenLengthOnly = filter (not. odd . length)

appendStr2Int :: [String] -> [Integer] -> [String]
appendStr2Int xs [] = []
appendStr2Int [] ys = []
appendStr2Int (x:xs) (y:ys) =  (x ++ show y) : appendStr2Int xs ys

appendStr2Int' :: [String] -> [Integer] -> [String]
appendStr2Int' = zipWith (\x y -> x ++ show y)

addList :: [Integer] -> Integer
addList [] = 0
addList (x:xs) = x + addList xs

findString :: String -> [String] -> Bool
findString s [] = False
findString s (x:xs) = s == x || findString s xs

fold :: (a -> b -> b) -> b -> [a] -> b
fold rf bc []     = bc
fold rf bc (x:xs) = rf x (fold rf bc xs)

findString' :: String -> [String] -> Bool
findString' s xs = fold (\x y -> x == s || y) (False) xs

addList' :: [Integer] -> Integer
addList' xs = fold (+) (0) xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = fold (\x y -> f x : y) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) = if p x then x : filter' p xs else filter' p xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p xs = fold (\x y -> if p x then x : y else y) [] xs
