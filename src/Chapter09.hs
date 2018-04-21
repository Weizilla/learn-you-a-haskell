module Chapter09 where

import Data.Char

sayHello :: String -> String
sayHello x = "Hello " ++ x

main' :: IO ()
main' = do
    putStrLn "What is your first name"
    firstName <- getLine
    putStrLn "What's your last name"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ " hey " ++ bigFirstName ++ " " ++ bigLastName ++ " whats up"

main'' = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs
