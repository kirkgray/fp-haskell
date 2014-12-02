compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t ( t x )

type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

myiterate :: (a -> a) -> a -> [a]
myiterate f = unfold (const False) id f

myevens :: [Integer] -> [Integer]
myevens [] = []
myevens xs = [ x | x <- xs, x `mod` 2 == 0]

mysquares :: Integer -> [Integer]
mysquares 0 = []
mysquares n = [ x ^ 2 | x  <- [1..n] ]

sumsquares :: Integer -> Integer
sumsquares n = sum (mysquares n)

mysquares2 :: Integer -> Integer -> [Integer]
mysquares2 0 y = []
mysquares2 x y = [ x ^ 2 | x  <- [(y + 1)..(x + y)] ]

sumsquares2 :: Integer -> Integer
sumsquares2 x = sum . uncurry mysquares2 $ (x, x)

coords :: Integer -> Integer -> [(Integer, Integer)]
coords a b = [ (x,y) | x <- [0..a], y <- [0..b] ]

myall :: (a -> Bool) -> [a] -> Bool
--myall p xs = and (map p xs)
--myall p = and . map p
--myall p = not . any ( not . p )
--myall p xs = foldl (&&) True (map p xs)
myall p = foldr (&&) True . map p

myany :: ( a -> Bool ) -> [a] -> Bool
--myany p = or . map p
--myany p xs = length ( filter p xs ) > 0
--myany p = not . null . dropWhile ( not . p )
--myany p = null . filter p
--myany p xs = not ( all (\ x -> not ( p x ) ) xs)
--myany p xs = foldr ( \ x acc -> ( p x ) || acc) False xs
myany p xs = foldr (||) True ( map p xs )

-- Take While
mytw :: (a -> Bool) -> [a] -> [a]
mytw _ [] = []
mytw p (x : xs)
    | p x = x : mytw p xs
    | otherwise = []

-- Drop While
mydr :: (a -> Bool) -> [a] -> [a]
mydr _ [] = []
mydr p ( x : xs )
    | p x = mydr p xs
    | otherwise = x : xs

-- Map
mymap :: (a -> b) -> [a] -> [b]
mymap f = foldl (\ xs x -> xs ++ [f x]) []

--filter
myfi :: (a -> Bool) -> [a] -> [a]
myfi p = foldr (\ x xs -> if p x then x : xs else xs) []

dec2int :: [Integer] -> Integer
dec2int = foldl ( \ x y -> 10 * x + y ) 0

-- curry
myc :: ((a,b) -> c) -> a -> b -> c
myc f = \ x y -> f ( x, y )





