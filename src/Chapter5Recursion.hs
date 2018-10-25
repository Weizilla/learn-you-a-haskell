module Chapter5Recursion where

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
    | x < maxTail = maxTail
    | otherwise = x
  where
    maxTail = maximum' xs

replicate' :: (Ord n, Num n) => n -> a -> [a]
replicate' n x
    | n <= 0 = [x]
    | otherwise = x : replicate' (n - 1) x

take' :: (Ord n, Num n) => n -> [a] -> [a]
take' _ [] = []
take' n _
    | n <= 0 = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
    | n == x = True
    | otherwise = elem' n xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smaller = quicksort [n | n <- xs, n < x]
        bigger = quicksort [n | n <- xs, n >= x]
     in smaller ++ [x] ++ bigger
