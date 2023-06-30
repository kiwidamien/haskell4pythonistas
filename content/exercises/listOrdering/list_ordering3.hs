module Sublist (sublist) where

import Data.List (tails)


sublist :: (Eq a) => [a] -> [a] -> Maybe Ordering
sublist x y 
  | xs == ys = Just EQ
  | (xs `elem` (tails ys)) = Just LT
  | (ys `elem` (tails xs)) = Just GT
  | otherwise = Nothing
