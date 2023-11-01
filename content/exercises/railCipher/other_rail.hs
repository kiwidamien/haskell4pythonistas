module RailFenceCipher (encode, decode) where
import Data.List (sortOn)

encode :: Int -> [a] -> [a]
encode n = zipSort $ cycle $ [1..n] ++ reverse [2..n-1]

decode :: Int -> [a] -> [a]
decode n xs = zipSort (encode n [1..length xs]) xs

zipSort :: Ord a => [a] -> [b] -> [b]
zipSort ns xs = map snd $ sortOn fst $ zip ns xs