import Data.List

type Square = [[Int]]

{-
 - A slow way of generating magic squares
 -
 - Requires constuctions of all (n^2)! permutations of an nxn
 - array, then summing rows, columns, and diagonals to filter
 - out the squares that don't satify the "same sum" property.
 -}

makeSquare :: Int -> [Int] -> Square
makeSquare n [] = []
makeSquare n ns = (take n ns) : makeSquare n (drop n ns)


sumMainDiagonal :: Square -> Int
sumMainDiagonal sq = sum diag
  where diag = map (\(i, line) -> line!!i) $ zip [0..] sq

sumSecondaryDiagonal :: Square -> Int
sumSecondaryDiagonal sq = sum diag
  where diag = map (\(i, line) -> line!!i) $ zip [0..] $ reverse sq 


isValid :: Square -> Bool
isValid sq = (length $ nub $ concat [
  map (sum) sq,
  map (sum) (transpose sq),
  [sumMainDiagonal sq],
  [sumSecondaryDiagonal sq]
  ]) == 1


allSquares :: Int -> [Square]
allSquares n = filter isValid $ map (makeSquare n) $ permutations [1..(n*n)] 
