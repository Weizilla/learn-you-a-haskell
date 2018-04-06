module Chapter05 where

l = [1, 2, 3, 4, 5]

l2 = [6, 7, 8, 9, 10]

l3 = [2, 4, 6, 4, 2]

max' :: (Ord a) => [a] -> a
max' [] = error "maximum of empty list"
max' [x] = x
max' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
  where
    maxTail = max' xs

max'' :: (Ord a) => [a] -> a
max'' [] = error "maximum of empty list"
max'' [x] = x
max'' (x:xs) =
    let maxTail = max'' xs
     in if x > maxTail
            then x
            else maxTail

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n xs
    | n <= 0 = []
take' n [] = []
take' n (x:xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [i | i <- xs, i <= x]
        biggerSorted = quicksort [i | i <- xs, i > x]
     in smallerSorted ++ [x] ++ biggerSorted
