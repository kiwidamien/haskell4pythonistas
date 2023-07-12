import Data.List

smallestMaybeList :: Maybe [Integer] -> Maybe[Integer] -> Maybe[Integer]
smallestMaybeList (Just a) Nothing = (Just a)
smallestMaybeList Nothing (Just b) = (Just b)
smallestMaybeList Nothing Nothing = Nothing
smallestMaybeList (Just a) (Just b) = if (length a) < (length b) then (Just a) else (Just b)



makeChange :: Integer -> [Integer] -> Maybe [Integer]
makeChange 0 _ = Just []
makeChange _ [] = Nothing
makeChange amount coins
  | amount < 0 = Nothing
  | amount `elem` coins = Just [amount]
  | otherwise = foldl' smallestMaybeList Nothing [(++) [c] <$> makeChange (amount-c) remCoin | c <- remCoin]
      where remCoin = [c | c <- coins, c < amount]

