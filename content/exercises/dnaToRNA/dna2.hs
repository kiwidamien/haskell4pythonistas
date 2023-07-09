module DNA (toRNA) where

import Data.Map (Map)
import qualified Data.Map as Map

pureAppendChar :: String -> Char -> String
pureAppendChar s c = s ++ [c]

reduceList :: [Maybe Char] -> Maybe String 
reduceList cs = foldl appendChar (Just "") cs
    where appendChar s c = pureAppendChar <$> s <*> c

toRNA :: String -> Maybe String
toRNA xs = reduceList $ map (\x -> Map.lookup x translation) xs
    where translation = Map.fromList [('A','U'),('C','G'),('G','C'),('T','A')]
