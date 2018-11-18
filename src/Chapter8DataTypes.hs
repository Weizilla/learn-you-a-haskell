module Chapter8DataTypes where

data Shape
    = Circle Point
             Float
    | Rectangle Point
                Point
    deriving (Show)

data Point =
    Point Float
          Float
    deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
    (abs $ x2 - x1) * (abs $ y2 - y1)

data Person = Person
    { firstName :: String
    , lastName :: String
    , age :: Int
    } deriving (Show)

p = Person {firstName = "Joe", lastName = "Smith", age = 10}

data Vector a =
    Vector a
           a
           a
    deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector x y z) (Vector x2 y2 z2) = Vector (x + x2) (y + y2) (z + z2)
