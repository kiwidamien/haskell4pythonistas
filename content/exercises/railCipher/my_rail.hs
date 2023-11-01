module RailFenceCipher (encode, decode) where
import Data.Char (isSpace)
import Data.List (sort)

process :: String -> String
process = filter (not . isSpace)

_steps :: Int -> [Int]
_steps n = cycle ([0..(n-1)] ++ reverse [1..(n-2)])

collapse ::  [(Char, Int)] -> Int -> [Char]
collapse charRail railNum = map fst $ filter (\(_, p)->(p==railNum)) charRail

expand :: [Char] -> Int -> [[Char]]
expand encoded numRails = [collapse e n| n<- [0..(numRails-1)]]
    where e = zip encoded $ sort $ take (length encoded) $ _steps numRails

encode :: Int -> String -> String
encode numRail plain = concat [collapse charRail n|n<-[0..(numRail-1)]]
  where charRail = zip (process plain) (_steps numRail)

decode :: Int -> String -> String
decode numRails cipher = go (_steps numRails) expanded
  where expanded = expand cipher numRails
        go (o:os) [] = []
        go (o:os) rails = if (length (rails!!o)==0) then [] else (head (rails!!o)) : go (os) (remaining o rails)
            where remaining o rails = [if n==o then tail (rails!!n) else rails!!n|n<-[0..(numRails-1)]]
