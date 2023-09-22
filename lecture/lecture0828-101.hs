-- Lecture 08.28

multiplyOut :: [Integer] -> Integer
multiplyOut [] = 1
multiplyOut (x:xs) = x * multiplyOut xs

addLengths :: [String] -> Integer
addLengths [] = 0
addLengths (x:xs) = addLengths xs + fromIntegral (length x)

-- return true if every element in the list is an even number
allEvens :: [Integer] -> Bool
allEvens [] = True
-- allEvens (x:xs) = if even x then allEvens xs else False
-- allEvens (x:xs) = allEvens xs && even x
allEvens (x:xs) = even x && allEvens xs

-- String = [Char]
concatStrings :: [String] -> String
concatStrings [] = []
concatStrings (s:ss) = s ++ concatStrings ss

concatLists :: [[a]] -> [a]
concatLists [] = []
concatLists (l:ls) = l ++ concatLists ls

-- Given a list of pairs [(Integer,String)], output the list of strings
-- in which the string is truncated at the given level
-- [(3,"hello"),(2,"there"),(5,"world")]   ~~~>>   ["hel","th","world"]

-- helper to cut off a string at a given point
cutOff :: Integer -> String -> String
cutOff 0 s = ""
cutOff k (c:s) = c : cutOff (k-1) s

cutOffAtGivenPoint :: [(Integer,String)] -> [String]
cutOffAtGivenPoint [] = []
cutOffAtGivenPoint ((i,s):xs) = (cutOff i s) : cutOffAtGivenPoint xs
