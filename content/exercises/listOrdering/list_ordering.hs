module Sublist (sublist) where

{-
 - This is the code more or less "translated" from the Python
 - version
 -}

listStartsWith :: (Eq a) => [a] -> [a] -> Bool
listStartsWith [] _ = True
listStartsWith _ [] = False
listStartsWith (x:xs) (y:ys)
  | x /= y = False
  | otherwise = listStartsWith xs ys


isSublist :: (Eq a) => [a] -> [a] -> Bool
isSublist [] [] = True
isSublist _ [] = False
isSublist x y = (listStartsWith x y) ||  (isSublist x (tail y))


sublist :: (Eq a) => [a] -> [a] -> Maybe Ordering
sublist xs ys 
  | xs == ys = Just EQ
  | (isSublist xs ys) = Just LT
  | (isSublist ys xs) = Just GT
  | otherwise = Nothing
