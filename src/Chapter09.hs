module Chapter09 where

import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.IO

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

main7 =
    forever $ do
        l <- getContents
        putStrLn $ map toUpper l

main8 = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\l -> length l < 10) allLines
        result = unlines shortLines
     in result

main9 = interact $ unlines . filter ((< 15) . length) . lines

responsePalindromes contents = unlines $ map output $ lines contents
  where
    output xs =
        if xs == reverse xs
            then "palindrome"
            else "not palindrome"

main10 = interact responsePalindromes

main11 = do
    handle <- openFile "test-data.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

main12 =
    withFile
        "test-data.txt"
        ReadMode
        (\handle -> do
             contents <- hGetContents handle
             putStr contents)

doTest = do
    getLine

doTest2 = do
    return "a" :: IO String

doTest3 = do
    do do do return "a" :: IO String

readTest =
    withFile
        "test-data.txt"
        ReadMode
        (\handle -> do
             contents <- hGetContents handle
             putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

main13 = do
    withFile'
        "test-data-2.txt"
        ReadMode
        (\h -> do
             contents <- hGetContents h
             putStr contents)

main14 = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")

main15 = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ "-" ++ line) [0 ..] todoTasks
    putStrLn "These are your todos"
    putStr $ unlines numberedTasks
    putStrLn "Which ones do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"

main16 = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The program args are:"
    mapM_ putStrLn args
    putStrLn "The program name is:"
    putStrLn progName
