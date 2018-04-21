module Chapter06 where

import Data.List

l = [1 .. 5]

l2 = [1,3 .. 10]

l3 = [20,19 .. 0]

allL = l ++ l2 ++ l3

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multThreeTwo = multThree 2

plusFive :: (Num a) => a -> a
plusFive = (+ 5)

applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<= x) xs)
        biggerSorted = quicksort (filter (> x) xs)
     in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999 ..])
  where
    p x = x `mod` 3892 == 0

sumOddSquares :: (Integral a) => a
sumOddSquares = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
    | even x = x : chain (x `div` 2)
    | otherwise = x : chain (x * 3 + 1)

-- Can't do "(Int a) => a" because Int is a type. Only type classes can be constraints
largeChains :: Int
largeChains = (length (filter long (map chain [1 .. 100])))
  where
    long c = length c > 15

listOfFuns :: (Num a, Enum a) => [(a -> a)]
listOfFuns = map (*) [0 ..]

largeChains' :: Int
largeChains' = length (filter (\c -> length c > 15) (map chain [1 .. 100]))

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f = \x y -> f y x

-- fold left: (((0 + 1) + 2) + 3) \acc x
-- fold right: (0 + (1 + (2 + 3))) \x acc
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y =
    foldl
        (\acc x ->
             if x == y
                 then True
                 else acc)
        False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

max' :: (Ord a) => [a] -> a
max' =
    foldr1
        (\x acc ->
             if x > acc
                 then x
                 else acc)

max'' :: (Ord a) => [a] -> a
max'' =
    foldl1
        (\acc x ->
             if x > acc
                 then x
                 else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f =
    foldr
        (\x acc ->
             if f x
                 then x : acc
                 else acc)
        []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

tail' :: [a] -> a
tail' = foldl1 (\_ x -> x)

-- takeWhile will stop but filter will not for infinite list
sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

-- "reverse . sort" creates a new function, then apply it to allL
reverseAll = reverse . sort $ allL

-- eq to this
reverseAll' = f allL
  where
    f = reverse . sort

-- apply sort first, then apply reverse
reverseAll'' = reverse $ sort $ allL

-- eq to this
reverseAll''' = reverse (sort allL)

-- various ways of writing
-- sumOddSquares = sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))
sumOddSquares' :: Integer
sumOddSquares' = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]

sumOddSquares'' :: Integer
sumOddSquares'' =
    let squares = map (^ 2) [1 ..]
        odds = filter odd squares
        lessThan10000 = takeWhile (< 10000) odds
        sums = sum lessThan10000
     in sums
