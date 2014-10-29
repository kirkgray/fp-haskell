-- tuple version
add :: (Int, Int) -> Int
add (x,y) = x + y

-- curried version
add' :: Int -> (Int -> Int)
add' x y = x + y

-- curried 3 Int multiplier
mult' :: Int -> Int -> Int -> Int
mult' x y z = x * y * z

-- sum :: Num a => [a] -> a
-- sum