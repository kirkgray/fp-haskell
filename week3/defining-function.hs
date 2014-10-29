signum :: Int -> Int
signum n = if n < 0 then -1 else
            if n == 0 then 0 else 1

halve1 :: [a] -> ([a], [a])
halve1 xs = splitAt( length xs `div` 2 ) xs

halve2 :: [a] -> ([a], [a])
halve2 xs = (take (n `div` 2) xs, drop ( n `div` 2) xs)
    where n = length xs

halve3 :: [a] -> ([a], [a])
halve3 xs = splitAt( length xs `div` 2 ) xs

halve4 :: [a] -> ([a], [a])
halve4 xs = splitAt( div ( length xs ) 2 ) xs

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where n = length xs `div` 2

safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else tail xs

safetail2 :: [a] -> [a]
safetail2[] = []
safetail2 (_ : xs) = xs

safetail3 :: [a] -> [a]
safetail3 xs
    | null xs = []
    | otherwise = tail xs

safetail :: [a] -> [a]
safetail
    = \ xs ->
        case xs of
            [] -> []
            (_ : xs) -> xs

remove :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop ( n + 1 ) xs

funct :: Int -> [a] -> [a]
funct x xs = take ( x + 1 ) xs ++ drop x xs

e3 :: Int -> Int
e3 x = x * 2

e11 :: (Char,Bool)
e11 = ('\a', False) -- works
-- e11 = ("t", False)
-- e11 = ('ac', True)

e12 :: [(Char,Int)]
e12 = [('a', 1)]

e13 :: Int -> Int -> Int
-- e13 x y = x / y -- should be `div`
e13 x y = x + y * y


e14 :: ([Char],[Float])
--e14 = [('C', 0.1)]
e14 = ("Haskell",[1.1,2.2])

e15 :: [a] -> [b] -> (a,b)
--e15 x y = (y, x)
e15 xs ys = (head xs, head ys)


