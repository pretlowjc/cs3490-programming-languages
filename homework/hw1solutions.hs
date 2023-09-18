{-
BUBBLESORT
The bubblesort algorithm works by exchanging adjacent elements that are out
of order until all the elements are in the right order.
-}

{-
1. Write a function which recursively goes through the list and interchanges
every element that is followed by a smaller element.

bubble [1,3,2] = [1,2,3]
bubble [1,3,5,4,3,2] = [1,3,4,3,2,5]
-} 

bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:ys) | x <= y = x : bubble (y:ys)
                | x  > y = y : bubble (x:ys)
{-
2. Write a funciton which repeatedly applies the bubble operator to the given
list until it has no effect. (So that calling bubble results in the same list.)
-}

bubbleSort :: Ord a => [a] -> [a]
bubbleSort l = if l == l' then l else bubbleSort l' where l' = bubble l

{-
GENERATING, SEARCHING, AND REPLACING genSubstring
If 'u' is a string, then 'v' is a substring if 'u' can be written as 'u = xvy' 
for some strings 'x' and 'y'. The strings 'x' and 'y' can be empty. In particular, 
'ba' is a substring of 'abcba', as is 'abcba'. Note also that the empty string 
is a substring of every string. 
-}

{-
1. Write a function which returns true if the first argument is a substring
of the second.

isSubstring "aba" "abcba" = False
isSubstring "bc" "abcba" = True

Hint. Use a helper function 'isPrefix' defined in the previous homework, together
with recursive descent through the tail of the list.
-}

isPrefix :: String -> String -> Bool
isPrefix s t =    s == take (length s) t

isSubstring :: String -> String -> Bool
isSubstring ""    "" = True
isSubstring (_:_) "" = False
isSubstring s     t  = isPrefix s t || isSubstring s (tail t)

{-
2. Write a function which generates all non-empty prefixes of the given string and
puts them in a list.

genPrefix "" = []
genPrefix "hey" = ["h","he","hey"]

Hint. First, write a helper function that generates all the tails of the given
list, so that e.g., 'gentails "hey" = ["hey", "ey", "y"]'. This can be done with
straightforward recursion. Then use this function in combination wiht 'reverse' to
define 'genPrefix'.
-}

genPrefix :: String -> [String]
genPrefix ""     = []
genPrefix (x:xs) = map reverse (helper [x] xs)
  where helper p [] = [p]
        helper p (x:xs) = p : helper (x:p) xs

{-
3. Write a function which generates all the substrings of a given string and puts
them in a list.

genSubstrings "abcd" = ["","a","ab","b","abc","bc","c","abcd","bcd","cd","d"]

You may list the substrings in a different order, but your list should avoid any 
duplicate entries.

Hint. Use 'genPrefix' as a helper function.
-}

genSubstrings :: String -> [String]
genSubstrings [] = [""]
genSubstrings (x:xs) = genPrefix (x:xs) ++ genSubstrings xs

{-
4. Write a function which takes a pair '(old, new)' of strings, another string 
'str', and replaces the prefix 'old' of 'str' with the string 'new'.

The function can have undefined behavior if 'old' is not a prefix of 'str'.

replacePrefix ("be", "spe") "bear" = "spear"

Hint. There is a simple solution to this question using the 'drop' function.
-}

replacePrefix :: (String,String) -> String -> String
replacePrefix (x,y) s = y ++ drop (length x) s

{-
5. Write a function which replaces the first occurence of a substring with a new
string:

replaceString ("morning", "evening") "Good morning!" = "Good evening!"
-}

replaceString :: (String,String) -> String -> String
replaceString _ "" = ""
replaceString (x,y) s | isPrefix x s = replacePrefix (x,y) s
replaceString (x,y) s = head s : replaceString (x,y) (tail s)

{-
A SIMPLE CYPHER

A substitution cypher is a simple encoding technique where each character in the 
input text is mapped to another character according to some fixed permutation of
the alphabet. We can represent such a permutation by a lookup table - a list of
key-value pairs.
-}

{- 
1. Write a function so that calling 'lookUp x perm' searches the list 'perm' for
the pair '(x,y)' whose first coordinate is the given character 'x'. It should then
output the second coordinate 'y'.

lookUp ’C’ [(’A’,’X’),(’B’,’Y’),(’C’,’Z’),(’D’,’W’)] = ’Z’
-}

lookUp :: Char -> [(Char,Char)] -> Char
lookUp c [] = error "No character found in the table."
lookUp c ((x,y):ts) | c == x = y
lookUp c ((x,y):ts) | c /= x = lookUp c ts

{-
Write a function which takes a lookup table and a string and replaces every character
by the value it is mapped to by the given table.

encode [(’A’,’X’),(’B’,’Y’),(’C’,’Z’),(’D’,’W’)] "BABA" = "YXYX"

(How could you implement the decode function?)
-}

encode :: [(Char,Char)] -> String -> String
encode perm "" = ""
encode perm (x:xs) = lookUp x perm : encode perm xs

{-
Write a function which takes two strings and creates a table by pairing up their
characters at the same position.

makeTable "ABCD" "XYZW" = [(’A’,’X’),(’B’,’Y’),(’C’,’Z’),(’D’,’W’)]
-}

makeTable :: String -> String -> [(Char,Char)]
makeTable "" _ = []
makeTable _ "" = []
makeTable (x:xs) (y:ys) = (x,y) : makeTable xs ys

{-
Once you implemented the functions above, you can make a Caesar cypher by rotating
the English alphabet by a fixed number of letters:

caesar :: Int -> [(Char,Char)]|
caesar n = makeTable abc (drop n (cycle abc)) where abc = [’A’..’Z’]
-}
