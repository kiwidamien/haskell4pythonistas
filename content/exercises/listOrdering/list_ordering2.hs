module Sublist (sublist) where

import Data.List (tails)


isSublist :: (Eq a) => [a] -> [a] -> Bool
isSublist x y = x `elem` (map (\t -> take (length x) t) $ tails y) 


sublist :: (Eq a) => [a] -> [a] -> Maybe Ordering
sublist xs ys 
  | xs == ys = Just EQ
  | (isSublist xs ys) = Just LT
  | (isSublist ys xs) = Just GT
  | otherwise = Nothing
