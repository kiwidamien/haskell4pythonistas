module SumOfMultiples (sumOfMultiples) where
import qualified Data.Set as S

multiples :: Integer -> [Integer]
multiples n = m
    where m = n : zipWith (+) (m) (repeat n)

-- note: moved multiples inside uniqueMultiplesLess Than
uniqueMultiplesLessThan :: [Integer] -> Integer -> [Integer]
uniqueMultiplesLessThan factors limit = S.toList $ S.fromList m
    where m = concat $ map (\n -> takeWhile (< limit) $ multiples n) factors


sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ uniqueMultiplesLessThan factors limit