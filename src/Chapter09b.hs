module Chapter09b where

import Control.Monad

main' = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main'

main'' = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a, b, c]

main = do
    colors <-
        forM
            [1, 2, 3, 4]
            (\a -> do
                 putStrLn $ "Which color do you associate with this number " ++ show a ++ "?"
                 color <- getLine
                 return color)
    putStrLn "The results"
    mapM_ putStrLn colors
