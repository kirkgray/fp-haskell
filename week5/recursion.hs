factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial ( n - 1 )

theproduct :: [Int] -> Int
theproduct[] = 1
theproduct (n:ns) = n * theproduct ns

thelength :: [a] -> Int
thelength[] = 0
thelength (_:xs) = 1 + thelength xs

thezip :: [a] -> [b] -> [(a,b)]
thezip[] _ = []
thezip _ [] = []
thezip (x:xs) (y:ys) = (x,y) : thezip xs ys

quicksort :: [Int] -> [Int]
quicksort[] = []
quicksort (x:xs) =
    quicksort smaller ++ [x] ++ quicksort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

toreplicate :: Int -> a -> [a]
toreplicate n x
    | n <= 0 = []
    | otherwise = x : toreplicate( n - 1 ) x

myexp :: Int -> Int -> Int
myexp m 0 = 1
myexp m 1 = m
--myexp m n = m *


mylength :: [a] -> Int
mylength [] = 0
mylength (_ : xs) = 1 + mylength xs

mydrop :: Int -> [a] -> [a]
mydrop 0 xs = xs
mydrop n [] = []
mydrop n (_ : xs) = mydrop (n - 1) xs

myinit :: [a] -> [a]
myinit [_] = []
myinit (x : xs) = x : myinit xs


myand :: [Bool] -> Bool
myand [] = True
myand (b : bs) = b && myand bs

myand2 :: [Bool] -> Bool
myand2 [] = True
myand2 (b : bs)
    | b = myand2 bs
    | otherwise = False


myand3 :: [Bool] -> Bool
myand3 [] = False
myand3 (b : bs) = b || and bs

myand4 :: [Bool] -> Bool
myand4 []  = True
myand4 (b : bs)
    | b == False = False
    | otherwise = myand4 bs

myand5 :: [Bool] -> Bool
myand5 [] = True
myand5 (b : bs) = b || myand5 bs

myand6 :: [Bool] -> Bool
myand6 [] = True
myand6 ( b : bs ) = myand6 bs && b

myand7 :: [Bool] -> Bool
myand7 [] = True
myand7 ( b : bs )
    | b = b
    | otherwise = myand7 bs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat ( xs : xss ) = xs ++ myconcat xss

myrepl :: Int -> a -> [a]
myrepl 0 _ = []
myrepl n x = x : myrepl ( n - 1 ) x

--(*&) :: [a] -> Int -> a
--(x : _ ) *& 0 = [x]
--( _ : xs ) *& n = xs *& ( n - 1 )

myelem :: Eq a => a -> [a] -> Bool
myelem _ [] = False
myelem x ( y : ys )
    | x == y = True
    | otherwise = myelem x ys

mymerge :: Ord a => [a] -> [a] -> [a]
mymerge [] ys = ys
mymerge xs [] = xs
mymerge ( x : xs ) ( y : ys ) = if x <= y then x : mymerge xs (y : ys) else y : mymerge (x : xs) ys

halve :: [a] -> ([a],[a])
halve xs = splitAt( length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = mymerge (msort ys) (msort zs)
    where (ys,zs) = halve xs













