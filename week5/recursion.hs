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
myexp m n = m * 
