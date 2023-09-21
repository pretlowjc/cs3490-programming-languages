{-
	This file is used for practicing and learning Haskell and its syntax.
-}

-- Functions 
addOne :: Int -> Int
addOne int = int + 1

timesThree :: Int -> Int
timesThree x = x * 3

sayHello :: String -> String
sayHello target = "Hello, " ++ target ++ "!"




-- Multiple Argument Functions
add :: Int -> Int -> Int -- 1
add first second = first + second -- 2

three :: Int 
three = add 1 2 -- 3

{- 
1 We don’t need parenthesis around the output because the -> operator is 
right-associative. This means that is it interpreted as, Int -> (Int -> Int). 

2 The process of supplying less than the full set of arguments is called currying, 
and is used very frequently in Haskell.

3 We don’t need parenthesis around add 1, as they are implied. Function application 
is left-associative. For example, the expression a b c d is interpreted as ((a b) c) d.
-}




-- Pattern Matching and Branching
