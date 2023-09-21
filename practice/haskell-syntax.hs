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

fib :: Int -> Int -- 1
fib 0 = 1 -- 2
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2) -- 3

{-
1 Always remember to include a type declaration for all your top-level values and 
functions!

2 In this piece of our declaration, we’ve replaced the argument to fib with a literal 
pattern. If the argument to fib is equal to 0, then this part of the declaration is 
executed. Otherwise, it passes through to the next declaration. This is somewhat like
a switch statement in some imperative languages, with the caveat that exactly one of 
the branches is taken.

3 The last declaration uses a variable n as the pattern. When you see a variable in 
a pattern, it will match anything, and bind the value that it matched to that variable. 
Thus, it resembles the function declarations we saw earlier.
-}

cases :: (Int, Int) -> String
cases tuple =
  case tuple of -- 1
    (0, 0) -> "Nones"
    (1, 1) -> "Ones"
    (1, _) -> "First one"
    _ -> "Other"

{-
1 The specific indentation follows the Haskell layout rules. The case often appears 
on the same line as the function declaration, but the beginnings of the different 
cases must be indented past the case and aligned.
-}

comparison :: Int -> Int -> String
comparison a b -- 1
  | a < b = "Less" -- 2
  | a > b = "Greater"
  | otherwise = "Equal" -- 3

{-
1 Instead of immediately declaring the result, divide it into several cases, each 
prefixed by |. 

2 Each case starts with |, followed by a condition (an expression of type Bool), 
then the rest of the declaration.

3 The otherwise keyword is used for the fall-through default case.
-}

-- Comparison can also be written with if statements.

comparison :: Int -> Int -> String
comparison a b =
  if a < b
  then "Less"
  else
    if a > b
    then "Greater"
    else "Equal"