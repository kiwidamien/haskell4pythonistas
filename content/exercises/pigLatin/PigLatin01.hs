module PigLatin01 (translate) where 
import Control.Monad (when)
import Data.ByteString.Builder (word16Dec, word16BE)

startsWithVowelSound :: String -> Bool
startsWithVowelSound "" = False
startsWithVowelSound (x:y:_)
  | elem x "aeiou" = True
  | (x == 'x') && (y == 'r') = True
  | (x == 'y') && (y == 't') = True
  | otherwise = False
startsWithVowelSound (x:[]) = elem x "aeiou"

moveInitialConsonant :: String -> String 
moveInitialConsonant w
  | startsWithVowelSound w == True = w
  | otherwise = rest ++ initial ++ "ay"
  where initial = getConsonantCluster w
        rest = drop (length initial) w


getConsonantCluster :: String -> String
getConsonantCluster word = go "" word
  where go acc "" = acc
        go acc (first:rest) 
           | (first == 'u') && (length acc > 0) && (last acc == 'q') = acc ++ "u"          | startsWithVowelSound word = acc
           | (first == 'y') && (length acc > 0) = acc
           | startsWithVowelSound (first:rest) = acc 
           | otherwise = go (acc ++ [first]) rest


translateWord :: String -> String
translateWord w 
  | startsWithVowelSound w = w ++ "ay"
  | otherwise = moveInitialConsonant w

translate :: String -> String
translate msg = unwords $ map translateWord $ words msg