module Chapter8DataTypes where

import qualified Data.Map as Map

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

data Point = Point Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Person = Person
    { firstName :: String
    , lastName :: String
    , age :: Int
    } deriving (Show)

p = Person { firstName = "Joe", lastName = "Smith", age = 10 }

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector x y z) (Vector x2 y2 z2) = Vector (x + x2) (y + y2) (z + z2)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap
lockers = Map.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNum map = case Map.lookup lockerNum map of
    Nothing -> Left $ "Locker number " ++ show lockerNum ++ " doesn't exist"
    Just (state, code) -> if state /= Taken
        then Right code
        else Left $ "Locker number " ++ show lockerNum ++ " is taken"

infixr 5 :-:
data Lst a = Empty | a :-: (Lst a) deriving (Show, Eq)

infixr .++
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node n treeL treeR)
    | x < n = Node n (treeInsert x treeL) treeR
    | x > n = Node n treeL (treeInsert x treeR)
    | otherwise = Node n treeL treeR

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node n treeL treeR)
    | x < n = treeElem x treeL
    | x > n = treeElem x treeR
    | x == n = True

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False


instance Functor Tree where
    fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)
    fmap _ EmptyTree = EmptyTree
