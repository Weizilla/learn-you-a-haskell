module Chapter11 where

import Data.Char
import Data.List

reverseLine = do
    line <- getLine
    let line' = reverse line
    putStrLn $ "you said " ++ line' ++ " backwards!"

reverseLine' = do
    line <- fmap reverse getLine
    putStrLn $ "2 you said " ++ line ++ " backwards!"

exclaimationLine = fmap (++ "!") getLine

multiFunctionLine = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line
