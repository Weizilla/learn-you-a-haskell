sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
    name2 <- getLine
    putStrLn $ greeting name
    name3 <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs
