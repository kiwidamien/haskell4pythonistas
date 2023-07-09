module Triangle (rows) where

perow:: Int -> [Int]
perow n 
  | n < 0 = []
  | n == 0 = [1]
  | otherwise = nextRow $ perow (n-1)
  where nextRow prev = [1] ++ zipWith (+) prev (tail prev) ++ [1] 

-- Now generate the entire row, and select the desired element
-- 
pe :: Int -> Int -> Int
pe r c = (perow r) !! c 


rows :: Int -> [[Int]]
rows x = [perow n | n <- [0..x]]
