module Chapter09Random where

import Control.Monad (when)
import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
     in (firstCoin, secondCoin, thirdCoin)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
    let (value, newGen) = random gen
     in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Eq n, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n - 1) newGen
     in (value : restOfList, finalGen)

sysRandom :: IO ()
sysRandom = do
    gen <- getStdGen
    putStr $ take 20 (randomRs ('a', 'z') gen)

main :: IO ()
main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStrLn "Which num am I thinking of?"
    numberInput <- getLine
    when (not $ null numberInput) $ do
        let guess = read numberInput
        if guess == randNumber
            then putStrLn "You are correct!"
            else putStrLn $ "You are incorrect. Num was " ++ show randNumber
        askForNumber newGen
