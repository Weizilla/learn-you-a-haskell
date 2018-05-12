module Chapter10 where

import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction (x:y:xs) "*" = (x * y) : xs
    foldingFunction (x:y:xs) "+" = (x + y) : xs
    foldingFunction (x:y:xs) "-" = (x - y) : xs
    foldingFunction (x:y:xs) "/" = (x / y) : xs
    foldingFunction (x:y:xs) "^" = (y ** x) : xs
    foldingFunction (x:y:xs) "sum" = [sum xs]
    foldingFunction xs n = read n : xs

data Section = Section
    { getA :: Int
    , getB :: Int
    , getC :: Int
    } deriving (Show)

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon =
    [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 10]

data Label
    = A
    | B
    | C
    deriving (Show)

type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPriceToA = priceA + a
        crossoverPriceToA = priceB + b + c
        forwardPriceToB = priceB + b
        crossoverPriceToB = priceA + a + c
        newPathToA =
            if forwardPriceToA <= crossoverPriceToA
                then (A, a) : pathA
                else (C, c) : (B, b) : pathB
        newPathToB =
            if forwardPriceToB <= crossoverPriceToB
                then (B, b) : pathB
                else (C, c) : (A, a) : pathA
     in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
        priceA = sum $ map snd bestAPath
        priceB = sum $ map snd bestBPath
     in if priceA <= priceB
            then reverse bestAPath
            else reverse bestBPath

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a, b, c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path: " ++ show pathString
    putStrLn $ "The path price: " ++ show pathPrice
