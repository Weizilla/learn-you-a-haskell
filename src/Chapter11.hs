module Chapter11 where

main' :: IO ()
main' = do
    line <- getLine
    let line' = reverse line
    putStrLn $ "You said " ++ line' ++ " backwards"

main'' :: IO ()
main'' = do
    line' <- fmap reverse getLine
    putStrLn $ "You said " ++ line' ++ " backwards'"
