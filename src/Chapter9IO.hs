module Chapter9IO where

import Data.Char

printStuff :: IO ()
printStuff = do
    putStrLn "Hello"
    firstName <- getLine
    let lastName = "Taco"
    putStrLn $ "Hello " ++ firstName ++ " " ++ lastName

reverseAll :: IO ()
reverseAll = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            reverseAll

reverseWords :: String -> String
reverseWords = unwords . map reverse . words