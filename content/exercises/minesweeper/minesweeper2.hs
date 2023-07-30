module Minesweeper (annotate) where
import Data.Char

type Point = (Int, Int)

neighborhood :: Point -> [[a]] -> [a]
neighborhood _ [] = []
neighborhood (row, col) grid = [grid !! r !! c | r<-[(row-1)..(row+1)], c<-[(col-1)..(col+1)], (r,c)/=(row,col), r>=0, r<(length grid), c>=0, c<length(grid!!0)]

scoreCell :: Point -> [[Char]] -> Int
scoreCell pt grid = length $ filter ('*'==) $ neighborhood pt grid

scoreGrid :: [[Char]] -> [[Int]]
scoreGrid [] = []
scoreGrid grid = [[scoreCell (row, col) grid | col<-[0..(numCols-1)]] | row <- [0..(numRows-1)]]
     where numCols = length $ head grid 
           numRows = length grid 

numsToString :: [[Int]] -> [String] -> [String]
numsToString = zipWith (zipWith resultToChar)
       where resultToChar _ '*' = '*'
             resultToChar 0 _ = ' '
             resultToChar n _ = chr(ord('0') + n) 

annotate :: [String] -> [String]
annotate [] = []
annotate grid = numsToString (scoreGrid grid) grid