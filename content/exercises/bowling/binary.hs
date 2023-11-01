module BinarySearch (find) where

import Data.Array

inBounds:: Ord a => Int -> Int -> Array Int a -> a -> Maybe Int
inBounds lower upper arr value
  | midpoint == value = Just offset
  | midpoint < (arr ! lower) = Nothing
  | midpoint > (arr ! upper) = Nothing
  | midpoint < value = inBounds midpoint upper arr value
  | midpoint > value = inBounds lower midpoint arr value
  where offset = div (lower + upper) 2
        midpoint = arr ! offset

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = inBounds 0 ((length array)-1) array x