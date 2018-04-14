module Chapter08 where

import qualified Data.Map as Map

data Point =
    Point Float
          Float
    deriving (Show)

data Shape
    = Circle Point
             Float
    | Rectangle Point
                Point
    deriving (Show)

c = Circle (Point 10.0 15.0) 5.0

r = Rectangle (Point 1.0 2.0) (Point 3.0 4.0)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) nx ny = Circle (Point (x + nx) (y + ny)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) nx ny =
    Rectangle (Point (x1 + nx) (y1 + ny)) (Point (x2 + nx) (y2 + ny))

p = Person {firstName = "bob", lastName = "smith", age = 22}

p2 = Person {firstName = "a", lastName = "b", age = 10}

p3 = Person {firstName = "a", lastName = "b", age = 10}

p4 = Person {firstName = "a", lastName = "b", age = 10}

data Person = Person
    { firstName :: String
    , lastName :: String
    , age :: Int
    } deriving (Show, Eq, Read, Ord)

car = Car {company = "Ford", model = "E", year = 1999}

data Car = Car
    { company :: String
    , model :: String
    , year :: Int
    } deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) =
    "This " ++ c ++ " model " ++ m ++ " on year " ++ show y

data Vector a =
    Vector a
           a
           a
    deriving (Show)

v1 = Vector 1 2 3

v2 = Vector 2 3 4

v3 = Vector 3 4 5

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) = Vector (i + l) (j + m) (k + n)

vmult :: (Num t) => Vector t -> t -> Vector t
vmult (Vector i j k) l = Vector (i * l) (j * l) (k * l)

vscalar :: (Num t) => Vector t -> Vector t -> t
vscalar (Vector i j k) (Vector l m n) = i * l + j * m + k * n

data Day
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun
    deriving (Show, Enum, Read, Ord, Eq, Bounded)

days = [minBound .. maxBound] :: [Day]

type PhoneNumber = String

type Name = String

type PhoneBook = AssocListStr String

pb = [("a", "1"), ("b", "2"), ("c", "3")]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnum pbook = (name, pnum) `elem` pbook

type AssocList k v = [(k, v)]

type AssocListStr v = AssocList String v

data LockerState
    = Empty
    | Taken
    deriving (Show, Eq)

type Code = String

type LockerNum = Int

type LockerMap = Map.Map LockerNum (LockerState, Code)

lockerLookup :: LockerNum -> LockerMap -> Either String Code
lockerLookup lockerNum lockerMap =
    case Map.lookup lockerNum lockerMap of
        Nothing -> Left $ "Locker number " ++ show lockerNum ++ " does not exist"
        Just (state, code) ->
            if state /= Taken
                then Right code
                else Left $ "Locker number " ++ show lockerNum ++ " is already taken"

lockers :: LockerMap
lockers =
    Map.fromList
        [ (100, (Taken, "ABC"))
        , (200, (Taken, "DEF"))
        , (300, (Empty, "HIJ"))
        , (400, (Empty, "KLM"))
        , (500, (Taken, "NOP"))
        ]

infixr 5 :-:

data List' a
    = EmptyList
    | a :-: (List' a)
    deriving (Show, Read, Eq, Ord)

infixr 5 .++

(.++) :: List' a -> List' a -> List' a
EmptyList .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

l = 1 :-: 2 :-: 3 :-: EmptyList

l2 = 5 :-: 6 :-: 7 :-: EmptyList

data Tree a
    = EmptyTree
    | Node a
           (Tree a)
           (Tree a)
    deriving (Show, Eq, Read)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node n left right)
    | x == n = Node n left right
    | x < n = Node n (treeInsert x left) right
    | x > n = Node n left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node n left right)
    | x == n = True
    | x < n = treeElem x left
    | x > n = treeElem x right

t = foldr treeInsert EmptyTree [3, 5, 7, 2, 4, 6]

data TrafficLight
    = Red
    | Yellow
    | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "RED LIGHT"
    show Green = "GREEN LIGHT"
    show Yellow = "YELLOW LIGHT"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf input trueResult falseResult =
    if yesno input
        then trueResult
        else falseResult
