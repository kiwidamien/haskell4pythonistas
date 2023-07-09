module Triangle (rows) where

pe :: Int -> Int -> Int
pe r c = if (c==0) || (c==r) then 1 else ((pe (r-1) (c-1)) + (pe (r-1) c))


perow :: Int -> [Int]
perow n 
  | n < 0 = []
  | otherwise = [pe n c| c <- [0..n]]


rows :: Int -> [[Int]]
rows x = [perow n | n <- [0..x]]
