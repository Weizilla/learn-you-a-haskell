module Chapter9IO where

import Data.Char
import Control.Monad

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

doColors :: IO ()
doColors = do
  colors <- forM [1, 2, 3, 4] (\a -> do
    putStrLn $ "What color for " ++ show a ++ "?"
    getLine)
  putStrLn "These are your colors:"
  mapM_ putStrLn colors

shortLines :: IO ()
shortLines = do
  contents <- getContents
  putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      short = filter (\l -> length l < 10) allLines
      result = unlines short
  in result