module SumOfMultiples (sumOfMultiples) where
import qualified Data.Set as S

-- use the slightly less efficent way
multiplesUpTo :: Integer -> Integer -> [Integer]
multiplesUpTo base limit = [mult*base | mult <- [0..limit], mult*base < limit]

-- note: moved multiples inside uniqueMultiplesLess Than
uniqueMultiplesLessThan :: [Integer] -> Integer -> [Integer]
uniqueMultiplesLessThan factors limit = S.toList $ S.fromList m
    where m = concat $ map (\n -> multiplesUpTo n limit) factors


sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ uniqueMultiplesLessThan factors limit