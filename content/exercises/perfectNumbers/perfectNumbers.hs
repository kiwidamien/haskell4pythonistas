module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

factors :: Int -> [Int]
factors n = [f | f<-[1..n], mod n f == 0]

classify :: Int -> Maybe Classification
classify n 
    | n < 1 = Nothing
    | 2*n == sumOfFactors = Just Perfect
    | 2*n > sumOfFactors = Just Deficient
    | otherwise = Just Abundant
    where sumOfFactors = sum $ factors n
