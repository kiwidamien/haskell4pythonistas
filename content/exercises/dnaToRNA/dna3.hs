module DNA (toRNA) where

import Data.Map (Map)
import qualified Data.Map as Map

pureAppendChar :: String -> Char -> String
pureAppendChar s c = s ++ [c]

reduceList :: [Either String Char] -> Either String String 
reduceList cs = foldl appendChar (Right "") cs
    where appendChar s c = pureAppendChar <$> s <*> c

translate :: Char -> Either String Char
translate 'A' = Right 'U'
translate 'C' = Right 'G'
translate 'G' = Right 'C'
translate 'T' = Right 'A'
translate c = Left ("Cannot translate " ++ [c])

toRNA :: String -> Either String String 
toRNA xs = reduceList $ map translate xs
