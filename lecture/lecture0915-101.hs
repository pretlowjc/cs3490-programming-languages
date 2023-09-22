i :: a -> a
i x = x    -- i = id

k :: a -> b -> a
k x y = x  -- k = const

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

b :: (b -> c) -> (a -> b) -> a -> c
b x y z = x (y z)  -- b = (.)

c :: (a -> b -> c) -> b -> a -> c
c x y z = x z y    -- c = flip

-- 1 or $
app :: (a -> b) -> (a -> b)
app x y = x y    -- ($)

-- concat the length at the end of each string
addLength :: [String] -> [String]
-- addLength = map (\x -> x ++ show (length x))
addLength = map (s (++) ((.) show length))
