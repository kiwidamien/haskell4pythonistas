module DNA (toRNA) where

import Data.Map (Map)
import qualified Data.Map as Map

type ErrMsg = String

pureAppendChar :: String -> Char -> String
pureAppendChar s c = s ++ [c]

reduceList :: [Either ErrMsg Char] -> Either ErrMsg String 
reduceList cs = foldl appendChar (Right "") cs
    where appendChar s c = pureAppendChar <$> s <*> c

charLookup = Map.fromList [('A', Right 'U'), ('C', Right 'G'), ('G', Right 'C'), ('T', Right 'A')]

translate :: Char -> Either ErrMsg Char
translate c = Map.findWithDefault (Left ("Cannot translate " ++ [c])) c charLookup 

toRNA :: String -> Either ErrMsg String 
toRNA xs = reduceList $ map translate xs
