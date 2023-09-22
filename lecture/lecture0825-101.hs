-- Lecture 08.25

{-
This is
a longer
comment
-}

n :: Integer
n = 7

m :: Int
m = 5

mul10 :: Int -> Int
mul10 x = 10 * x

mulBypi :: Double -> Double
mulBypi x = x * pi

myString :: [Char]
myString = "hello"

-- conditional
fact :: Integer -> Integer
fact n = if n == 0 then 1 else fact (n-1) * n

-- guards
fact2 :: Integer -> Integer
fact2 n | n <= 0    = 1
fact2 n             = fact2 (n-1) * n

-- pattern-matching
fact3 :: Integer -> Integer
fact3 0 = 1
fact3 n = n * fact3 (n-1)

-- using let
fact4 :: Integer -> Integer
fact4 0 = 1
fact4 n = let m = fact4 (n-1)
           in n * m

factMaybe :: Maybe Integer -> Integer
factMaybe Nothing = 0
factMaybe (Just n) | n <= 0 = 1
factMaybe (Just n) | otherwise = n * factMaybe (Just (n-1))

factMaybe' :: Maybe Integer -> Integer
factMaybe' Nothing = 0
factMaybe' (Just n) = fact3 n

distance :: Double -> Double -> Double -> Double
distance x y z = sqrt (x*x + y*y + z*z)

angle :: Double -> Double -> Double
angle x y = atan (y/x)

angle2 :: (Double,Double) -> Double
angle2 z = atan (snd z / fst z)

angle3 :: (Double,Double) -> Double
angle3 (x,y) = atan (y / x)

angle4 :: (Double,Double) -> Double
angle4 z = let (x,y) = z
            in atan (y/x)

angle5 :: (Double,Double) -> Double
angle5 z = atan (y/x) where (x,y) = z

sumOdds :: Integer -> Integer
sumOdds 0 = 0
sumOdds 1 = 1
sumOdds n | odd n = n + sumOdds (n-2)
sumOdds n | even n = sumOdds (n-1)
