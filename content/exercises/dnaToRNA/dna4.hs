module DNA (toRNA) where

import Data.Map (Map)
import qualified Data.Map as Map

type ErrMsg = String

pureAppendChar :: String -> Char -> String
pureAppendChar s c = s ++ [c]

reduceList :: [Either ErrMsg Char] -> Either ErrMsg String 
reduceList cs = foldl appendChar (Right "") cs
    where appendChar s c = pureAppendChar <$> s <*> c

translate :: Char -> Either ErrMsg Char
translate 'A' = Right 'U'
translate 'C' = Right 'G'
translate 'G' = Right 'C'
translate 'T' = Right 'A'
translate c = Left ("Cannot translate " ++ [c])

toRNA :: String -> Either ErrMsg String 
toRNA xs = reduceList $ map translate xs
