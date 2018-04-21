module Chapter09 where

import Control.Monad
import Data.Char

sayHello :: String -> String
sayHello x = "Hello " ++ x

main1 :: IO ()
main1 = do
    putStrLn "What is your first name"
    firstName <- getLine
    putStrLn "What's your last name"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ " hey " ++ bigFirstName ++ " " ++ bigLastName ++ " whats up"

main2 = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main2

main3 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main3
        else return ()

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs

main4 = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main4

main5 = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a, b, c]

main6 = do
    colors <-
        forM
            [1, 2, 3, 4]
            (\a -> do
                 putStrLn $ "Which color do you associate with this number " ++ show a ++ "?"
                 color <- getLine
                 return color)
    putStrLn "The results"
    mapM_ putStrLn colors

main = main6
