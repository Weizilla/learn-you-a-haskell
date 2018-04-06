module Chapter04 where

sayMe :: (Integral a, Show a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe x = "Say " ++ show x

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (a, b) (c, d) = (a + c, b + d)

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

l = [(1, 2), (3, 4), (5, 6)]

head' :: [a] -> a
head' [] = error "Can't call on empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "There is one element " ++ show x
tell [x, y] = "There are two elements " ++ show x ++ " " ++ show y
tell (x:y:_) = "There are more than two elements " ++ show x ++ " " ++ show y

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string!"
capital all@(x:xs) = "First letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight"
    | bmi <= 25.0 = "You're normal"
    | bmi <= 30.0 = "You're fat"
    | otherwise = "You're obese"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight"
    | weight / height ^ 2 <= 25.0 = "You're normal"
    | weight / height ^ 2 <= 30.0 = "You're fat"
    | otherwise = "You're obese"

bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
    | bmi <= skinny = "Skinny"
    | bmi <= normal = "Normal"
    | bmi <= fat = "Fat"
    | otherwise = "Obese"
  where
    bmi = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials first second = [f] ++ " first " ++ [s] ++ " second"
  where
    (f:_) = first
    (s:_) = second

initials' :: String -> String -> String
initials' (f:_) (s:_) = [f] ++ " first " ++ [s] ++ " second"

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi h w | (h, w) <- xs] -- bmi is a function here
  where
    bmi height weight = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
     in sideArea + topArea * 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2] -- bmi is a value here

calcBmis'' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis'' xs = [w / h ^ 2 | (w, h) <- xs]

describeList :: [a] -> String
describeList xs =
    "This list is " ++
    case xs of
        [] -> "empty"
        [x] -> "a single element list"
        [x, y] -> "two item list"
        (x:y:_) -> "more than two item list"

-- "what xs" in "where" is same as a function defination with pattern matching
describeList' :: [a] -> String
describeList' xs = "This list is " ++ what xs
  where
    what [] = "empty"
    what [x] = "single item"
    what [x, y] = "two items"
    what xs = "longer"
