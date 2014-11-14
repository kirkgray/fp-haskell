import Data.Char

repli :: Int -> a -> [a]
repli n a = [a | _ <- [1 .. n]]

pyths :: Int -> [(Int, Int, Int)]
--pyths n = [(x,y,z) | x <- [1..n], y <- [1..x], z <- [1 .. y], x ^ 2 + y ^ 2 == z ^ z]
--pyths n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n], x ^ 2 + y ^ 2 == z ^ 2]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]
--pyths n = [(x,y, (x ^ 2 + y ^ 2)) | x <- [1..n], y <- [1.n]]

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
--perfects n = [x | x <- [1..n], isPerfect x] where isPerfect num = sum( factors num ) == num
perfects n = [x | x <- [1..n], isPerfect x]
    where isPerfect num = sum( init (factors num)) == num

--ex4 :: Int -> [(Int,Int)]
--ex4 = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (l, v) <- t, k == l]

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
    where n = length xs - 1

scalarproduct :: [ Int ] -> [ Int ] -> Int
scalarproduct xs ys = sum [ x * y | (x,y) <- xs `zip` ys]



let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr( ord 'a' + n )

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let (( let2int c + n ) `mod` 26)
    | otherwise = c
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0 

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `divides` d]




