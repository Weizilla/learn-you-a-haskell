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
