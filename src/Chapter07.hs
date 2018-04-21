module Chapter07 where

import Data.Char
import Data.Function
import Data.List

words' :: String -> [String]
words' = filter (not . any isSpace) . groupBy ((==) `on` isSpace)
-- can't do this because the last (groupBy...) isn't a complete expression since
-- groupBy is missing the input list parameter
-- words' = filter (not . any isSpace) (groupBy ((==) `on` isSpace))
-- if want the group by as a separate term with (), need to have input list (xs)
-- words' xs = filter (not . any isSpace) (groupBy ((==) `on` isSpace) xs)
