module Minesweeper (annotate) where
import Data.Char

neighborhood :: Int -> Int -> [[a]] -> [a]
neighborhood _ _ [] = []
neighborhood row col grid = [grid !! r !! c | r<-[(row-1)..(row+1)], c<-[(col-1)..(col+1)], (r,c)/=(row,col), r>=0, r<(length grid), c>=0, c<length(grid!!0)]

scoreCell :: Int -> Int -> [[Char]] -> Int
scoreCell row col grid = length $ filter ('*'==) $ neighborhood row col grid

scoreGrid :: [[Char]] -> [[Int]]
scoreGrid [] = []
scoreGrid grid = [[scoreCell row col grid | col<-[0..(numCols-1)]] | row <- [0..(numRows-1)]]
     where numCols = length $ head grid 
           numRows = length grid 

numsToString :: [[Int]] -> [String] -> [String]
numsToString nums origMap = map (\(lineNums, lineChars) -> map (\(n,s)-> resultToChar n s) $ zip lineNums lineChars) $ zip nums origMap
    where digitToChar d = chr(ord('0') + d)
          resultToChar n s = if s=='*' then '*' else if n==0 then ' ' else (digitToChar n) 


annotate :: [String] -> [String]
annotate [] = []
annotate grid = numsToString (scoreGrid grid) grid