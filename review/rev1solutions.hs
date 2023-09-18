{-
1. Write a function which takes two floating point numbers, x and y, and returns
the distance from the point (x,y) to the origin.
-}

radius :: Double -> Double -> Double
radius x y = sqrt (x*x + y*y)

{-
2. Write a function which operates the same way as above, but it takes as input the
pair (x,y) as a single argument.
-}

radius' :: (Double,Double) -> Double
radius' (x,y) = sqrt (x*x + y*y)

{-
3. Write a function which adds up all the even numbers from 1 to its input argument.

sumEvens 5 = 6
sumEvens 8 = 20

Do not use list comprehension or Haskell's built-in "sum" function.
-}

sumEvens :: Integer -> Integer
sumEvens n | n <= 1 = 0
sumEvens n | even n = n + sumEvens (n-2)
sumEvens n | odd n  = sumEvens (n-1)

{-
4. Write a function which does the same thing as the previous one, but now using list
comprehension and other functions.
-}

sumEvens' :: Integer -> Integer
sumEvens' n = sum [0,2..n]

{-
5. The Collatz function is defined as following:

f(n) =
1 n = 0 or n = 1
f(n/2) n > 1 is even
f(3n + 1) n > 1 is odd

Write a Haskell function so that collatz n equals f(n) for every n >= 0.
-}

collatz :: Integer -> Integer
collatz n | n <= 1 = 1
collatz n | even n = collatz (div n 2)
collatz n | odd  n = collatz (3*n + 1)

{-
6. The Collatz Conjecture states that f(n) always returns 1 when n > 0. No one
knows if this is true. You will be very famous if you solve this problem, see 
'https://en.wikipedia.org/wiki/Collatz_conjecture'.

Verify that the Collatz conjecture is true for 1 <= n <= 100, by constructing a 
list consisting of values of f(n) in that range.
-}

collatzCheck :: [Integer]
collatzCheck = [collatz n | n <- [1..100]]

{-
7. Using ranges and/or list comprehension, create a list of all numbers 1 <= n <= 100
divisible by 5. Save this as Haskell term 'multiplesOfFive :: [Integer].
-}

multiplesOfFive :: [Integer]
multiplesOfFive = [n | n <- [1..100], n `mod` 5 == 0]

{-
8. Using Haskell's built-in functions, write a function 'init'' which returns all 
but the last element. Do not use Haskell's built-in 'init' function.
-}

init' :: [a] -> [a]
init' xs = reverse (tail (reverse xs))

{-
9. Using Haskell's built-in functions, write a function that takes a list of strings, 
and determines whether it contains an empty string.

Hint. Remember that strings are really lists, so you can use functions like 'elem'
and 'null'.
-}

findEmpty :: [String] -> Bool
findEmpty [] = False
findEmpty (x:xs) = null x || findEmpty xs

{-
10. Using Haskell's built-in functions and/or list comprehension, write a function
which takes a list of strings and returns a list containing their lenghts:

getLengths ["Hello","World"] = [5,5]
-}

getLengths :: [String] -> [Int]
getLengths [] = []
getLengths (x:xs) = length x : getLengths xs
