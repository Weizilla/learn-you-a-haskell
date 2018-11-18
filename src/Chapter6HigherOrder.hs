module Chapter6HigherOrder where

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smaller = quicksort' $ filter (< x) xs
        bigger = quicksort' $ filter (>= x) xs
     in smaller ++ [x] ++ bigger

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys =
    foldl
        (\acc x ->
             if x == y
                 then True
                 else acc)
        False
        ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

maximum'' :: (Ord a) => [a] -> a
maximum'' =
    foldr1
        (\x acc ->
             if x > acc
                 then x
                 else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (Eq a) => (a -> Bool) -> [a] -> [a]
filter'' f =
    foldr
        (\x acc ->
             if f x
                 then x : acc
                 else acc)
        []
