module Chapter4Syntax where

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY"
lucky x = "Not lucky"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)

addVectors :: (Integral a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tail' :: [a] -> a
tail' [x] = x
tail' (x:xs) = tail' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "thin"
    | bmi <= normal = "normal"
    | bmi <= fat = "fat"
    | otherwise = "too fat"
  where
    bmi = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = 2 * pi * r ^ 2
     in sideArea + topArea * 2

describeList :: [a] -> String
describeList xs = "This list is " ++ what xs
  where
    what xs =
        case xs of
            [] -> "empty"
            [x] -> "single element"
            x -> "big list"

describeList' :: [a] -> String
describeList' xs = "This list is " ++ what' xs
  where
    what' [] = "empty"
    what' [x] = "single"
    what' x = "big list"
