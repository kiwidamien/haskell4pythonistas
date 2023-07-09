module DNA (toRNA) where

import Data.Map (Map)
import qualified Data.Map as Map

appendChar :: Maybe String -> Maybe Char -> Maybe String
appendChar Nothing _ = Nothing
appendChar _ Nothing = Nothing
appendChar (Just s) (Just c) = Just (s ++ [c])

reduceList :: [Maybe Char] -> Maybe String 
reduceList cs = foldl appendChar (Just "") cs

toRNA :: String -> Maybe String
toRNA xs = reduceList $ map (\x -> Map.lookup x translation) xs
    where translation = Map.fromList [('A','U'),('C','G'),('G','A'),('T','A')]
