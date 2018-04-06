module Chapter02 where

length' xs = sum [1 | _ <- xs]

removeLowerCase xs = [c | c <- xs, c `elem` ['A'..'Z']]

l = [1, 2, 3, 4, 5]
l2 = [6, 7, 8, 9, 10]

triangles = [(a, b, c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 24]

main :: IO()
main = do
  print (triangles)
