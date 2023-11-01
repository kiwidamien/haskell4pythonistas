module Garden
    ( Plant (..)
    , garden
    , lookupPlants
    ) where

import qualified Data.Map as M
import Data.List 

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)


{-
This is a pretty silly problem!

We have a string with two lines, containing only the characters CGRV
Each line has 2x the number of students, each of whom has distinct names.

So if 
    Roster = ["Alice", "Bob", "Charlie"]
Each line has 6 characters. The first two characters of each line are plants for Alice, the next two are for Bob, the last two are for Charlie.

e.g.

  plantMap arranged like
    AABBCC
    AABBCC   
  so 
  plantMap = "CGRVVV\nGGRRVV" 
  means 
   - Alice is taking care of Clover, Grass, Grass, Grass  (CGGG)
   - Bob is taking care of Radishes, Violets, Radishes, Radishe (RVRR)
   - Charlie is taking care of all Violets (VVVV)
-}

type Name = String
type Roster = [Name]
type PlantList = String 
type Garden =  M.Map Name [Plant]

unroll :: [String] -> Maybe(String, [String])
unroll pList 
  | all (=="") pList = Nothing
  | otherwise = Just (takeTopTwoEachLine pList, dropTwo pList)
    where takeTopTwoEachLine  = concatMap (take 2) 
          dropTwo = map (drop 2)


lookUp :: Char -> Plant
lookUp 'C' = Clover
lookUp 'G' = Grass 
lookUp 'R' = Radishes 
lookUp 'V' = Violets


garden :: Roster -> PlantList -> Garden
garden students plants = M.fromList $ map (\(name, pString) -> (name, map lookUp pString)) $ zip (sort students) (unfoldr unroll $ lines plants)


lookupPlants :: String -> Garden -> [Plant]
lookupPlants student garden = extract (M.lookup student garden)
    where extract (Just x) = x 
          extract (Nothing) = error "Student not in classroom"


exNames = words "Alice Bob Charlie"
exMap =  "CGRVVV\nGGRRVV" 
