module Chapter7Modules where

import Data.List
import qualified Data.Map as Map

phoneBook = [("123", "abc"), ("234", "bcd"), ("345", "cde"), ("456", "def")]

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey x =
    foldr
        (\(k, v) acc ->
             if x == k
                 then Just v
                 else acc)
        Nothing

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty
