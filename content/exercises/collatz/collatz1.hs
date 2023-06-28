module CollatzConjecture (collatz) where


step :: Integer -> Integer 
step n = if (mod n 2 == 0) then (div n 2) else (3*n + 1)


collatzSequence :: Integer -> [Integer]
collatzSequence n 
  | n < 2 = [n]
  | otherwise = n : (collatzSequence $ step n)


collatz :: Integer -> Maybe Integer
collatz n 
  | n < 1 = Nothing
  | otherwise = Just $ fromIntegral $ length $ collatzSequence n

{--
Cute approach I saw in one function in community solutions

collatz :: Integer -> Maybe Integer
collatz 1 = Just 0
collatz n
  | n <= 0 = Nothing
  | otherwise = (+1) <$> collatz (if n `mod` 2 == 0 then n `div` 2 else n * 3 + 1)

--}