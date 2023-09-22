addLists :: [Integer] -> [Integer] -> [Integer]
addLists xs [] = []
addLists [] ys = []
addLists (x:xs) (y:ys) = (x+y) : addLists xs ys

filterOdds :: [Integer] -> [Integer]
filterOdds [] = []
filterOdds (x:xs) = if x `mod` 2 /= 0 then x : filterOdds xs else filterOdds xs

filterMod3 :: [Integer] -> [Integer]
filterMod3 [] = []
filterMod3 (x:xs) =  if x `mod` 3 /= 0 then x : filterMod3 xs else filterMod3 xs

filterMod :: Integer -> [Integer] -> [Integer]
filterMod n [] = []
filterMod n (x:xs) = if x `mod` n /= 0 then x : filterMod n xs else filterMod n xs

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs :: [Integer]
fibs = 0 : 1 : addLists fibs (tail fibs)

fib' :: Integer -> Integer
fib' n = fibs !! (fromIntegral n)

zeroes :: [Integer]
zeroes = 0 : zeroes

addOne :: [Integer] -> [Integer]
addOne [] = []
addOne (x:xs) = x+1 : addOne xs

ints :: [Integer]
ints = 2 : addOne ints

sieve :: [Integer] -> [Integer]
sieve (n:xs) = n : sieve (filterMod n xs)

primes :: [Integer]
primes = sieve ints
