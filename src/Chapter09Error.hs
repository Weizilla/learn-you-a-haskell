module Chapter09Error where

import Control.Exception
import System.Environment
import System.IO
import System.IO.Error

toTry :: IO ()
toTry = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ "lines"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of
            Just path -> putStrLn $ "File does not exist at " ++ path
            Nothing -> putStrLn $ "File does not exist as unknown location"
    | otherwise = ioError e

main = toTry `catch` handler
